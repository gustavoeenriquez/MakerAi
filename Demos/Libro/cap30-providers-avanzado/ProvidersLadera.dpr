program ProvidersLadera;

// =============================================================================
// LIBRO "IA con Delphi" - capitulo 30: lo avanzado de cada proveedor, en consola
// =============================================================================
// La misma tienda, el mismo prompt largo (Lucia + el manual + las preguntas frecuentes,
// unos 5.000 tokens), y lo que cada proveedor ofrece por debajo de AddMessageAndRun:
//   --cache          el prefijo repetido se cobra como cache: Claude (CacheContext, escribe y lee),
//                    OpenAI y DeepSeek (automatico), Gemini (implicito). Tres llamadas por proveedor:
//                    la primera escribe, la segunda (chat nuevo) lee el prompt, la tercera lee
//                    tambien el historial. Tokens y ahorro estimado.
//   --citas          Claude Citations: el manual como documento de texto, cada afirmacion con su
//                    fragmento citado (lo garantiza el API, no el prompt)
//   --razonamiento   la misma pregunta con poco y con mucho razonamiento (ThinkingLevel) en OpenAI,
//                    Claude y DeepSeek: tokens de pensamiento, tiempo y respuesta
//   (sin argumentos: todo). Requiere OPENAI_API_KEY, CLAUDE_API_KEY, DEEPSEEK_API_KEY; GEMINI_API_KEY
//   si esta. Empresa, productos y precios ficticios.
// =============================================================================

{$APPTYPE CONSOLE}
{$R *.res}

uses
  {$IFDEF MSWINDOWS} Winapi.Windows, {$ENDIF}
  System.SysUtils,
  System.Classes,
  System.StrUtils,
  System.IOUtils,
  System.Diagnostics,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations;

const
  LUCIA = 'Eres Lucía, la barista virtual de Café La Ladera, una tienda de café de especialidad colombiano. ' +
    'Responde en español, cálido y breve (dos o tres frases). Solo hablas de café y de la tienda. No inventes productos ni precios. ' +
    'Abajo tienes el manual de la tienda y sus preguntas frecuentes; responde con lo que dicen.';

var
  BufSalida: array [0 .. 0] of Byte;
  Prefijo: string;   // Lucia + manual + preguntas frecuentes: lo que se repite en cada llamada

{ ---------------------------------------------------------------------------- apoyo }

function Tiene(const AArg: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to ParamCount do
    if SameText(ParamStr(I), AArg) then
      Exit(True);
end;

procedure Titulo(const ATexto: string);
begin
  Writeln;
  Writeln('== ' + ATexto + ' ' + StringOfChar('=', 76 - Length(ATexto)));
end;

function Carpeta: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
end;

function Conexion(const ADriver, AModelo, AKey: string): TAiChatConnection;
begin
  Result := TAiChatConnection.Create(nil);
  Result.DriverName := ADriver;
  Result.Model := AModelo;
  Result.Params.Values['ApiKey'] := AKey;
  Result.Params.Values['Asynchronous'] := 'False';
  Result.Params.Values['Max_Tokens'] := '4000';
  Result.AiChat.ResponseTimeOut := 120000;
  if SameText(ADriver, 'Claude') then
  begin
    // Sin busqueda web (Claude la declara por defecto y son tokens de mas); el conjunto completo y en los dos lados
    Result.ModelCaps := [cap_Image, cap_Pdf, cap_Reasoning];
    Result.SessionCaps := Result.ModelCaps;
  end;
  Result.SystemPrompt.Text := Prefijo;
end;

function Preguntar(Conn: TAiChatConnection; const APregunta: string; out AError: string): string;
begin
  AError := '';
  try
    Result := Conn.AddMessageAndRun(APregunta, 'user', []);
    if (Result = '') and (Conn.LastError <> '') then
      AError := Conn.LastError;
  except
    on E: Exception do
    begin
      Result := '';
      AError := E.Message;
    end;
  end;
end;

function Resumen(const ATexto: string; AMax: Integer = 110): string;
begin
  Result := ATexto.Replace(#13, ' ').Replace(#10, ' ').Replace('  ', ' ').Trim;
  if Length(Result) > AMax then
    Result := Copy(Result, 1, AMax) + '…';
end;

{ ---------------------------------------------------------------------------- A: cache }

type
  TProveedor = record
    Driver, Modelo, Key: string;
    Entrada, Cache, Salida: Double;   // USD por millon: entrada normal, entrada leida de cache, salida
    Nota: string;
  end;

const
  PROVEEDORES: array [0 .. 3] of TProveedor = (
    (Driver: 'Claude'; Modelo: 'claude-sonnet-5-5'; Key: '@CLAUDE_API_KEY'; Entrada: 2.00; Cache: 0.20; Salida: 10.00;
      Nota: 'CacheContext=True: el driver marca el prompt de sistema con cache_control; escribe (1,25x) y luego lee (0,1x)'),
    (Driver: 'OpenAi'; Modelo: 'gpt-6-sol'; Key: '@OPENAI_API_KEY'; Entrada: 2.00; Cache: 0.20; Salida: 10.00;
      Nota: 'automático: sin configurar nada, el prefijo repetido (≥ 1.024 tokens) vuelve como cached_tokens'),
    (Driver: 'DeepSeek'; Modelo: 'deepseek-flash'; Key: '@DEEPSEEK_API_KEY'; Entrada: 0.30; Cache: 0.003; Salida: 1.20;
      Nota: 'automático por prefijo: prompt_cache_hit_tokens; el precio de caché es 1 % del normal'),
    (Driver: 'Gemini'; Modelo: 'gemini-3.8-flash'; Key: '@GEMINI_API_KEY'; Entrada: 0.30; Cache: 0.03; Salida: 2.50;
      Nota: 'implícito: cachedContentTokenCount cuando el prefijo se repite; el explícito (CacheName) es aparte'));

procedure Cache;
const
  PREGUNTAS: array [0 .. 2] of string = ('¿Cuánto cuesta el envío a Bogotá y cuándo llega?',
    '¿Qué café me recomiendan para prensa francesa?', '¿Y cuánta cantidad de café uso por litro de agua?');
var
  P: TProveedor;
  Conn: TAiChatConnection;
  Msg: TAiChatMessage;
  I: Integer;
  R, Error: string;
  Reloj: TStopwatch;
  Normal, Real: Double;
begin
  Titulo('A. Caché: el mismo prompt largo en cuatro proveedores');
  Writeln(Format('  prefijo (Lucía + manual + preguntas frecuentes): %d caracteres', [Length(Prefijo)]));
  for P in PROVEEDORES do
  begin
    Writeln;
    Writeln(Format('  %s / %s — %s', [P.Driver, P.Modelo, P.Nota]));
    if GetEnvironmentVariable(P.Key.Substring(1)) = '' then
    begin
      Writeln('    (sin ' + P.Key.Substring(1) + ': se omite)');
      Continue;
    end;
    Conn := Conexion(P.Driver, P.Modelo, P.Key);
    try
      if SameText(P.Driver, 'Claude') then
        Conn.Params.Values['CacheContext'] := 'True';   // flag portable: en los que cachean solos no hace nada
      Normal := 0;
      Real := 0;
      Writeln('    llamada  entrada  escritas  leídas  salida  tiempo');
      for I := 0 to High(PREGUNTAS) do
      begin
        if I = 1 then
          Conn.NewChat;   // chat nuevo: solo se repite el prompt de sistema
        Reloj := TStopwatch.StartNew;
        R := Preguntar(Conn, PREGUNTAS[I], Error);
        if Error <> '' then
        begin
          Writeln('    error: ' + Resumen(Error, 160));
          Break;
        end;
        Msg := Conn.AiChat.GetLastMessage;
        Writeln(Format('    %7d  %7d  %8d  %6d  %6d  %5.1f s   %s', [I + 1, Msg.Prompt_tokens, Msg.Cache_write_tokens,
          Msg.Cached_tokens, Msg.Completion_tokens, Reloj.ElapsedMilliseconds / 1000, Resumen(R, 70)]));
        Normal := Normal + (Msg.Prompt_tokens + Msg.Cache_write_tokens + Msg.Cached_tokens) * P.Entrada / 1000000;
        Real := Real + (Msg.Prompt_tokens * P.Entrada + Msg.Cache_write_tokens * P.Entrada * 1.25 + Msg.Cached_tokens * P.Cache) / 1000000;
      end;
      if Error = '' then
        Writeln(Format('    entrada de las tres llamadas: US$ %.6f sin caché → US$ %.6f con caché (%.0f %% menos)',
          [Normal, Real, 100 - 100 * Real / Normal]));
    finally
      Conn.Free;
    end;
  end;
end;

{ ---------------------------------------------------------------------------- B: citas }

procedure Citas;
var
  Conn: TAiChatConnection;
  MF: TAiMediaFile;
  Msg: TAiChatMessage;
  Cit: TAiMsgCitation;
  R, Error: string;
  Reloj: TStopwatch;
begin
  Titulo('B. Citas: Claude responde con el fragmento exacto del manual');
  Conn := Conexion('Claude', 'claude-sonnet-5-5', '@CLAUDE_API_KEY');
  try
    Conn.SystemPrompt.Text := 'Eres Lucía, de Café La Ladera. Responde en español, breve, usando solo el documento adjunto.';
    // El manual como documento de texto: el driver lo manda como bloque document/text con citations activas.
    // El TAiMediaFile pasa a ser del mensaje: no se libera aparte.
    MF := TAiMediaFile.Create;
    MF.LoadFromFile(Carpeta + 'manual\ladera_tienda.txt');
    MF.EnableCitations := True;
    MF.Title := 'Manual de la tienda';
    Reloj := TStopwatch.StartNew;
    try
      R := Conn.AddMessageAndRun('¿Cuánto cuesta el envío a Bogotá y cuándo llega? ¿Y si el café llega con el empaque roto?', 'user', [MF]);
      Error := Conn.LastError;
    except
      on E: Exception do
        Error := E.Message;
    end;
    if (R = '') and (Error <> '') then
    begin
      Writeln('  error: ' + Resumen(Error, 200));
      Exit;
    end;
    Msg := Conn.AiChat.GetLastMessage;
    Writeln(Format('  %.1f s · %d tokens de entrada · %d de salida', [Reloj.ElapsedMilliseconds / 1000,
      Msg.Prompt_tokens + Msg.Cached_tokens + Msg.Cache_write_tokens, Msg.Completion_tokens]));
    Writeln('  Lucía: ' + R.Replace(sLineBreak, ' '));
    Writeln(Format('  citas: %d', [Msg.Citations.Count]));
    for Cit in Msg.Citations do
      Writeln(Format('    [%d..%d] «%s»', [Cit.StartIndex, Cit.EndIndex, Resumen(Cit.Text, 140)]));
  finally
    Conn.Free;
  end;
end;

{ ---------------------------------------------------------------------------- C: razonamiento }

procedure UnaCorrida(const ADriver, AModelo, AKey, ANivel: string; ACaps: TAiCapabilities; ALevel: TAiThinkingLevel; const APregunta: string);
var
  Conn: TAiChatConnection;
  Msg: TAiChatMessage;
  R, Error: string;
  Reloj: TStopwatch;
begin
  Conn := Conexion(ADriver, AModelo, AKey);
  try
    Conn.SystemPrompt.Text := LUCIA + ' Carta (bolsa de 340 g): Huila Reserva 38.000 COP; Nariño Altura 42.000 COP; ' +
      'Sierra Nevada Orgánico 36.000 COP; Descafeinado Suave 34.000 COP. Envío a Bogotá 9.000 COP; gratis desde 120.000 COP por pedido. ' +
      'Termina con una línea "Conviene: ..." y los dos totales.';
    // La receta de siempre: cap_Reasoning en las capacidades y el nivel en ThinkingLevel; el driver
    // lo traduce al contrato de cada API (effort en OpenAI, adaptive + effort en Claude, enabled + reasoning_effort en DeepSeek)
    Conn.ModelCaps := ACaps;
    Conn.SessionCaps := ACaps;
    Conn.ModelConfig.ThinkingLevel := ALevel;
    Reloj := TStopwatch.StartNew;
    R := Preguntar(Conn, APregunta, Error);
    if Error <> '' then
    begin
      Writeln(Format('  %-9s %-20s %-7s error: %s', [ADriver, AModelo, ANivel, Resumen(Error, 120)]));
      Exit;
    end;
    Msg := Conn.AiChat.GetLastMessage;
    Writeln(Format('  %-9s %-20s %-7s %6d  %6d  %8d  %5.1f s  %s', [ADriver, AModelo, ANivel, Msg.Completion_tokens,
      Msg.Thinking_tokens, Length(Msg.ReasoningContent), Reloj.ElapsedMilliseconds / 1000, Resumen(R, 60)]));
  finally
    Conn.Free;
  end;
end;

procedure Razonamiento;
const
  PREGUNTA = 'Una clienta de Bogotá quiere 3 bolsas de Nariño Altura. ¿Cuánto paga con envío si las pide juntas, ' +
    'y cuánto si pide 2 ahora y 1 el mes que viene? ¿Qué le conviene?';
begin
  Titulo('C. Razonamiento: la misma pregunta con poco y con mucho esfuerzo');
  Writeln('  ' + PREGUNTA);
  Writeln('  (juntas: 126.000, envío gratis · separadas: 93.000 + 51.000 = 144.000)');
  Writeln;
  Writeln('  driver    modelo               nivel   salida  pensar  razonam.  tiempo  respuesta');
  Writeln('                                          tokens  tokens  (chars)');
  UnaCorrida('OpenAi', 'gpt-6-sol', '@OPENAI_API_KEY', 'low', [cap_Reasoning], tlLow, PREGUNTA);
  UnaCorrida('OpenAi', 'gpt-6-sol', '@OPENAI_API_KEY', 'high', [cap_Reasoning], tlHigh, PREGUNTA);
  UnaCorrida('Claude', 'claude-sonnet-5-5', '@CLAUDE_API_KEY', 'low', [cap_Image, cap_Pdf, cap_Reasoning], tlLow, PREGUNTA);
  UnaCorrida('Claude', 'claude-sonnet-5-5', '@CLAUDE_API_KEY', 'high', [cap_Image, cap_Pdf, cap_Reasoning], tlHigh, PREGUNTA);
  UnaCorrida('DeepSeek', 'deepseek-flash', '@DEEPSEEK_API_KEY', 'off', [], tlDefault, PREGUNTA);
  UnaCorrida('DeepSeek', 'deepseek-flash', '@DEEPSEEK_API_KEY', 'high', [cap_Reasoning], tlHigh, PREGUNTA);
  Writeln('  "pensar" = tokens de razonamiento que reporta el API (OpenAI y DeepSeek); Claude los cobra dentro de la salida');
  Writeln('  "razonam." = largo del razonamiento devuelto (ReasoningContent): resumido en Claude, completo en DeepSeek, ninguno en OpenAI');
end;

{ ---------------------------------------------------------------------------- }

var
  Todo: Boolean;

begin
  {$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);
  SetTextCodePage(Output, CP_UTF8);
  {$ENDIF}
  SetTextBuf(Output, BufSalida, 1);
  if (GetEnvironmentVariable('OPENAI_API_KEY') = '') or (GetEnvironmentVariable('CLAUDE_API_KEY') = '') or
    (GetEnvironmentVariable('DEEPSEEK_API_KEY') = '') then
  begin
    Writeln('Faltan OPENAI_API_KEY, CLAUDE_API_KEY y/o DEEPSEEK_API_KEY.');
    ExitCode := 1;
    Exit;
  end;
  try
    Prefijo := LUCIA + sLineBreak + sLineBreak + TFile.ReadAllText(Carpeta + 'manual\ladera_tienda.txt', TEncoding.UTF8) +
      sLineBreak + sLineBreak + TFile.ReadAllText(Carpeta + 'manual\preguntas_frecuentes.txt', TEncoding.UTF8);
    Todo := not (Tiene('--cache') or Tiene('--citas') or Tiene('--razonamiento'));
    if Todo or Tiene('--cache') then
      Cache;
    if Todo or Tiene('--citas') then
      Citas;
    if Todo or Tiene('--razonamiento') then
      Razonamiento;
  except
    on E: Exception do
      Writeln(E.ClassName + ': ' + E.Message);
  end;
end.
