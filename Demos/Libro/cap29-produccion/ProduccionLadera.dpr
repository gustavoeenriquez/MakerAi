program ProduccionLadera;

// =============================================================================
// LIBRO "IA con Delphi" - capitulo 29: patrones de produccion, en consola
// =============================================================================
// Lo que separa una demo de una aplicacion que atiende clientes, con corridas reales:
//   --errores     un modelo que no existe (404: no se reintenta, responde el respaldo) y un
//                 servidor que no contesta (puerto cerrado: reintentos con espera creciente y
//                 despues el respaldo de otro proveedor)
//   --costos      tres turnos de Lucia: tokens por turno (entrada, cache, salida), acumulado de
//                 la conexion (Prompt_tokens...), costo estimado con la tarifa, ResetTokenCounters
//   --usuarios N  N clientes a la vez, cada uno con SU conexion, en hilos; tiempos y tokens
//   --ritmo       limite de peticiones por cliente y minuto, antes de llamar al API (sin costo)
//   (sin argumentos: todo). Requiere OPENAI_API_KEY y CLAUDE_API_KEY (el respaldo).
// Empresa, productos y precios ficticios.
// =============================================================================

{$APPTYPE CONSOLE}
{$R *.res}

uses
  {$IFDEF MSWINDOWS} Winapi.Windows, {$ENDIF}
  System.SysUtils,
  System.Classes,
  System.StrUtils,
  System.Diagnostics,
  System.SyncObjs,
  System.Generics.Collections,
  System.Net.HttpClient,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations;

const
  LUCIA = 'Eres Lucía, la barista virtual de Café La Ladera, una tienda de café de especialidad colombiano. ' +
    'Carta (bolsa de 340 g): Huila Reserva 38.000 COP (panela, frutos rojos); Nariño Altura 42.000 COP (cítricos, caramelo); ' +
    'Sierra Nevada Orgánico 36.000 COP (chocolate amargo, nuez); Descafeinado Suave 34.000 COP (cacao suave). ' +
    'Envíos: Bogotá, Medellín y Cali 2 días hábiles, 9.000 COP; resto del país 3 a 5 días, 14.000 COP; gratis desde 120.000 COP. ' +
    'Club La Ladera (suscripción): una bolsa de 340 g al mes por 34.000 COP con envío incluido, el café lo elige la catadora; se pausa o cancela sin penalidad. ' +
    'Reglas: responde en español, cálido y breve (dos o tres frases). Solo hablas de café y de la tienda. No inventes productos ni precios.';
  MAX_REINTENTOS = 3;

var
  BufSalida: array [0 .. 0] of Byte;

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

function ValorDe(const AArg: string; ADefault: Integer): Integer;
var
  I: Integer;
begin
  Result := ADefault;
  for I := 1 to ParamCount - 1 do
    if SameText(ParamStr(I), AArg) then
      Exit(StrToIntDef(ParamStr(I + 1), ADefault));
end;

procedure Titulo(const ATexto: string);
begin
  Writeln;
  Writeln('== ' + ATexto + ' ' + StringOfChar('=', 76 - Length(ATexto)));
end;

function Conexion(const ADriver, AModelo, AKey: string): TAiChatConnection;
begin
  // Sincrona: en consola se espera la respuesta. La clave va por variable de entorno (@NOMBRE):
  // nunca en el codigo, nunca en un archivo que se versione.
  Result := TAiChatConnection.Create(nil);
  Result.DriverName := ADriver;
  Result.Model := AModelo;
  Result.Params.Values['ApiKey'] := AKey;
  Result.Params.Values['Asynchronous'] := 'False';
  Result.AiChat.ResponseTimeOut := 90000;        // nunca una llamada sin limite (el minimo del framework: 61 s)
  Result.SystemPrompt.Text := LUCIA;
end;

function CodigoHttp(const AMensaje: string): Integer;
var
  P: Integer;
begin
  // El driver reporta "Error Received: 404, {...}". Sin codigo (DNS, red caida, puerto cerrado) es 0.
  Result := 0;
  P := Pos('Error Received: ', AMensaje);
  if P > 0 then
    Result := StrToIntDef(Copy(AMensaje, P + 16, 3), 0);
end;

function EsTransitorio(ACodigo: Integer): Boolean;
begin
  // Vale reintentar: sin respuesta (red), 408 timeout, 409, 429 rate limit, 5xx del servidor.
  // No vale: 400/401/403/404 (peticion, clave, permiso o modelo equivocados): fallan igual la segunda vez.
  Result := (ACodigo = 0) or (ACodigo = 408) or (ACodigo = 409) or (ACodigo = 429) or (ACodigo >= 500);
end;

function Preguntar(Conn: TAiChatConnection; const APregunta: string; out AError: string): string;
begin
  // En modo sincrono el framework NO relanza la excepcion del API: devuelve '' y deja el detalle en
  // LastError. Los errores de red (puerto cerrado, DNS) si pueden llegar como excepcion.
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

procedure QuitarPreguntaFallida(Conn: TAiChatConnection);
begin
  // La pregunta que fallo quedo en el historial (se agrega antes de llamar al modelo):
  // se quita para no mandarla dos veces
  if (Conn.Messages.Count > 0) and (Conn.Messages[Conn.Messages.Count - 1].Role = 'user') then
    Conn.Messages.Delete(Conn.Messages.Count - 1);
end;

function ConReintentos(Conn, Respaldo: TAiChatConnection; const APregunta: string): string;
var
  Intento, Codigo, Espera: Integer;
  Error: string;
  Reloj: TStopwatch;
begin
  // El patron completo: reintentar solo lo transitorio, con espera creciente (1 s, 2 s, 4 s), y si
  // no se puede, responder con OTRO proveedor. Al usuario le llega una respuesta, no un error.
  Reloj := TStopwatch.StartNew;
  for Intento := 1 to MAX_REINTENTOS do
  begin
    Result := Preguntar(Conn, APregunta, Error);
    if Error = '' then
    begin
      Writeln(Format('  respondió %s/%s en el intento %d (%.1f s)', [Conn.DriverName, Conn.AiChat.Model, Intento, Reloj.ElapsedMilliseconds / 1000]));
      Exit;
    end;
    Codigo := CodigoHttp(Error);
    Writeln(Format('  intento %d: %s%s', [Intento, IfThen(Codigo > 0, 'HTTP ' + IntToStr(Codigo) + ' · ', 'sin respuesta · '),
      Copy(Error.Replace(#13, ' ').Replace(#10, ' ').Replace('  ', ' '), 1, 120)]));
    QuitarPreguntaFallida(Conn);
    if not EsTransitorio(Codigo) then
    begin
      Writeln('  no es transitorio: no tiene sentido insistir');
      Break;
    end;
    if Intento < MAX_REINTENTOS then
    begin
      Espera := 1000 shl (Intento - 1);
      Writeln(Format('  espero %d s antes de volver a intentar', [Espera div 1000]));
      Sleep(Espera);
    end;
  end;
  Writeln(Format('  -> respaldo: %s/%s', [Respaldo.DriverName, Respaldo.AiChat.Model]));
  Respaldo.NewChat;
  Result := Preguntar(Respaldo, APregunta, Error);
  if Error <> '' then
    Result := '(tampoco el respaldo: ' + Copy(Error, 1, 120) + ')';
  Writeln(Format('  respondió el respaldo (%.1f s en total)', [Reloj.ElapsedMilliseconds / 1000]));
end;

{ ---------------------------------------------------------------------------- A: errores }

procedure Errores;
var
  Lucia, Respaldo: TAiChatConnection;
  R: string;
begin
  Titulo('A. Errores y reintentos');
  Respaldo := Conexion('Claude', 'claude-sonnet-5-5', '@CLAUDE_API_KEY');
  try
    // Sin busqueda web: Claude la declararia por defecto y cuesta tokens. OJO: el conjunto completo y en
    // los dos lados; si SessionCaps conserva cap_WebSearch, el hueco pide un puente que este driver no tiene.
    Respaldo.ModelCaps := [cap_Image, cap_Pdf, cap_Reasoning];
    Respaldo.SessionCaps := Respaldo.ModelCaps;

    Writeln('A1. Un modelo que no existe (gpt-nada): el API contesta 404 y no vale reintentar');
    Lucia := Conexion('OpenAi', 'gpt-nada', '@OPENAI_API_KEY');
    try
      R := ConReintentos(Lucia, Respaldo, '¿Cuál de sus cafés tiene notas de chocolate?');
      Writeln('  Lucía: ' + R.Replace(sLineBreak, ' '));
    finally
      Lucia.Free;
    end;

    Writeln;
    Writeln('A2. Un servidor que no contesta (puerto cerrado): transitorio, se reintenta con espera creciente');
    Lucia := Conexion('OpenAi', 'gpt-6-sol', '@OPENAI_API_KEY');
    try
      Lucia.AiChat.Url := 'http://127.0.0.1:9/';   // nadie escucha en el puerto 9: "connection refused"
      R := ConReintentos(Lucia, Respaldo, '¿Cuánto vale el envío a Medellín?');
      Writeln('  Lucía: ' + R.Replace(sLineBreak, ' '));
    finally
      Lucia.Free;
    end;
  finally
    Respaldo.Free;
  end;
end;

{ ---------------------------------------------------------------------------- B: costos }

function TarifaPorMillon(const AModelo: string; out AEntrada, ACache, ASalida: Double): Boolean;
begin
  // USD por millon de tokens (listas publicas, oct 2026): entrada, entrada leida de cache, salida
  Result := True;
  AEntrada := 0; ACache := 0; ASalida := 0;
  if AModelo.StartsWith('gpt-6-sol') then begin AEntrada := 2.00; ACache := 0.20; ASalida := 10.00; end
  else if AModelo.StartsWith('gpt-6-luna') then begin AEntrada := 0.10; ACache := 0.01; ASalida := 0.50; end
  else if AModelo.StartsWith('claude-sonnet-5') then begin AEntrada := 2.00; ACache := 0.20; ASalida := 10.00; end
  else if AModelo.StartsWith('deepseek') then begin AEntrada := 0.30; ACache := 0.003; ASalida := 1.20; end
  else Result := False;
end;

function CostoDe(const AModelo: string; AEntrada, ACache, ASalida: Integer): Double;
var
  E, C, S: Double;
begin
  Result := 0;
  if TarifaPorMillon(AModelo, E, C, S) then
    Result := (AEntrada * E + ACache * C + ASalida * S) / 1000000;
end;

procedure Costos;
const
  PREGUNTAS: array [0 .. 2] of string = ('¿Qué café me recomiendas para prensa francesa?',
    'Quiero dos bolsas de ese y una de Nariño Altura, ¿cuánto sería con envío a Bogotá?',
    '¿Y si me suscribo al club, qué me conviene más?');
var
  Lucia: TAiChatConnection;
  Msg: TAiChatMessage;
  I, Entrada, Cache, Salida: Integer;
  R, Error: string;
  Costo: Double;
  Reloj: TStopwatch;
begin
  Titulo('B. Costos: lo que cuesta una conversación');
  Lucia := Conexion('OpenAi', 'gpt-6-sol', '@OPENAI_API_KEY');
  try
    Entrada := 0; Cache := 0; Salida := 0; Costo := 0;
    Writeln('  modelo gpt-6-sol: US$ 2,00 por millón de tokens de entrada (0,20 en caché), 10,00 de salida');
    Writeln;
    Writeln('  turno  entrada  caché  salida  tiempo   costo turno   acumulado conexión (Prompt/Completion/Total)');
    for I := 0 to High(PREGUNTAS) do
    begin
      Reloj := TStopwatch.StartNew;
      R := Preguntar(Lucia, PREGUNTAS[I], Error);
      if Error <> '' then
      begin
        Writeln('  error: ' + Error);
        Exit;
      end;
      // El mensaje de respuesta trae el uso de ESTE turno (sumadas las rondas de herramientas, si hubo)
      Msg := Lucia.AiChat.GetLastMessage;
      Inc(Entrada, Msg.Prompt_tokens);
      Inc(Cache, Msg.Cached_tokens);
      Inc(Salida, Msg.Completion_tokens);
      Costo := Costo + CostoDe(Lucia.AiChat.Model, Msg.Prompt_tokens, Msg.Cached_tokens, Msg.Completion_tokens);
      // La conexion acumula sola: Prompt_tokens / Completion_tokens / Total_tokens de toda la sesion
      Writeln(Format('  %5d  %7d  %5d  %6d  %5.1f s  US$ %.6f   %d / %d / %d', [I + 1, Msg.Prompt_tokens, Msg.Cached_tokens,
        Msg.Completion_tokens, Reloj.ElapsedMilliseconds / 1000, CostoDe(Lucia.AiChat.Model, Msg.Prompt_tokens, Msg.Cached_tokens,
        Msg.Completion_tokens), Lucia.Prompt_tokens, Lucia.Completion_tokens, Lucia.Total_tokens]));
      Writeln('         Lucía: ' + Copy(R.Replace(sLineBreak, ' '), 1, 150) + IfThen(Length(R) > 150, '…', ''));
    end;
    Writeln;
    Writeln(Format('  sesión: %d de entrada (%d en caché) + %d de salida = %d tokens ≈ US$ %.6f', [Entrada, Cache, Salida,
      Entrada + Cache + Salida, Costo]));
    Writeln(Format('  Cost_usd de la conexión: %.6f (solo lo llena el driver MakerAI, que cobra en dólares; los demás, 0)', [Lucia.Cost_usd]));
    Writeln('  el historial crece: cada turno manda los anteriores; el prefijo repetido se cobra como caché');
    Lucia.ResetTokenCounters;
    Writeln(Format('  ResetTokenCounters -> Prompt_tokens %d, Completion_tokens %d, Total_tokens %d (el historial sigue: %d mensajes)',
      [Lucia.Prompt_tokens, Lucia.Completion_tokens, Lucia.Total_tokens, Lucia.Messages.Count]));
  finally
    Lucia.Free;
  end;
end;

{ ---------------------------------------------------------------------------- C: usuarios }

type
  TCliente = class(TThread)
  private
    FNombre, FPregunta, FRespuesta, FError: string;
    FMs: Int64;
    FTokens: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(const ANombre, APregunta: string);
  end;

constructor TCliente.Create(const ANombre, APregunta: string);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FNombre := ANombre;
  FPregunta := APregunta;
end;

procedure TCliente.Execute;
var
  Conn: TAiChatConnection;
  Reloj: TStopwatch;
begin
  // UNA conexion por cliente: el historial, el estado del turno y los contadores son suyos.
  // Compartir una TAiChatConnection entre usuarios mezcla las conversaciones y los hilos.
  Conn := Conexion('OpenAi', 'gpt-6-sol', '@OPENAI_API_KEY');
  try
    Reloj := TStopwatch.StartNew;
    FRespuesta := Preguntar(Conn, FPregunta, FError);
    FMs := Reloj.ElapsedMilliseconds;
    FTokens := Conn.Total_tokens;
  finally
    Conn.Free;
  end;
end;

procedure Usuarios(N: Integer);
const
  NOMBRES: array [0 .. 5] of string = ('Marta', 'Andrés', 'Camila', 'Jorge', 'Valentina', 'Pedro');
  PREGUNTAS: array [0 .. 5] of string = ('¿Cuál es el café más suave?', '¿Tienen descafeinado?',
    '¿Cuánto tarda un envío a Cali?', '¿Qué café tiene notas de caramelo?', '¿Desde cuánto el envío es gratis?',
    '¿Cuánto valen dos bolsas de Huila Reserva?');
var
  Clientes: TObjectList<TCliente>;
  C: TCliente;
  I, Tokens: Integer;
  Reloj: TStopwatch;
  Suma: Int64;
begin
  if N > Length(NOMBRES) then
    N := Length(NOMBRES);
  Titulo(Format('C. %d clientes a la vez, una conexión por cliente', [N]));
  Clientes := TObjectList<TCliente>.Create(True);
  try
    Reloj := TStopwatch.StartNew;
    for I := 0 to N - 1 do
      Clientes.Add(TCliente.Create(NOMBRES[I], PREGUNTAS[I]));
    Suma := 0;
    Tokens := 0;
    for C in Clientes do
    begin
      C.WaitFor;
      Inc(Suma, C.FMs);
      Inc(Tokens, C.FTokens);
    end;
    for C in Clientes do
      if C.FError <> '' then
        Writeln(Format('  %-10s %5.1f s  ERROR %s', [C.FNombre, C.FMs / 1000, Copy(C.FError, 1, 80)]))
      else
        Writeln(Format('  %-10s %5.1f s  %4d tokens  %s', [C.FNombre, C.FMs / 1000, C.FTokens,
          Copy(C.FRespuesta.Replace(sLineBreak, ' '), 1, 90)]));
    Writeln(Format('  en total %.1f s de reloj para %.1f s de respuestas sumadas: corrieron en paralelo (%d tokens)',
      [Reloj.ElapsedMilliseconds / 1000, Suma / 1000, Tokens]));
  finally
    Clientes.Free;
  end;
end;

{ ---------------------------------------------------------------------------- D: ritmo }

type
  TLimiteDeRitmo = class
  private
    FMaximo: Integer;
    FVentanaMs: Int64;
    FLlamadas: TDictionary<string, TList<Int64>>;
    FCandado: TCriticalSection;
  public
    constructor Create(AMaximo: Integer; AVentanaMs: Int64);
    destructor Destroy; override;
    function Permitir(const ACliente: string): Boolean;
  end;

constructor TLimiteDeRitmo.Create(AMaximo: Integer; AVentanaMs: Int64);
begin
  inherited Create;
  FMaximo := AMaximo;
  FVentanaMs := AVentanaMs;
  FLlamadas := TDictionary<string, TList<Int64>>.Create;
  FCandado := TCriticalSection.Create;
end;

destructor TLimiteDeRitmo.Destroy;
var
  L: TList<Int64>;
begin
  for L in FLlamadas.Values do
    L.Free;
  FLlamadas.Free;
  FCandado.Free;
  inherited;
end;

function TLimiteDeRitmo.Permitir(const ACliente: string): Boolean;
var
  L: TList<Int64>;
  Ahora: Int64;
begin
  // Ventana deslizante por cliente: cuenta las llamadas de los ultimos FVentanaMs; si llego al
  // maximo, se rechaza ANTES de gastar un token. Es local y barato; la decision es de la app.
  Ahora := TStopwatch.GetTimeStamp div (TStopwatch.Frequency div 1000);
  FCandado.Enter;
  try
    if not FLlamadas.TryGetValue(ACliente, L) then
    begin
      L := TList<Int64>.Create;
      FLlamadas.Add(ACliente, L);
    end;
    while (L.Count > 0) and (Ahora - L[0] > FVentanaMs) do
      L.Delete(0);
    Result := L.Count < FMaximo;
    if Result then
      L.Add(Ahora);
  finally
    FCandado.Leave;
  end;
end;

procedure Ritmo;
var
  Limite: TLimiteDeRitmo;
  I: Integer;
begin
  Titulo('D. Ritmo: máximo 5 peticiones por cliente cada minuto');
  Limite := TLimiteDeRitmo.Create(5, 60000);
  try
    for I := 1 to 7 do
      Writeln(Format('  Marta, petición %d: %s', [I, IfThen(Limite.Permitir('marta'), 'pasa', 'RECHAZADA (espere un momento)')]));
    Writeln(Format('  Andrés, petición 1: %s  (el límite es por cliente)', [IfThen(Limite.Permitir('andres'), 'pasa', 'RECHAZADA')]));
    Writeln('  ninguna de estas decisiones llamó al API');
  finally
    Limite.Free;
  end;
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
  if (GetEnvironmentVariable('OPENAI_API_KEY') = '') or (GetEnvironmentVariable('CLAUDE_API_KEY') = '') then
  begin
    Writeln('Faltan OPENAI_API_KEY y/o CLAUDE_API_KEY (el respaldo).');
    ExitCode := 1;
    Exit;
  end;
  try
    Todo := not (Tiene('--errores') or Tiene('--costos') or Tiene('--usuarios') or Tiene('--ritmo'));
    if Todo or Tiene('--errores') then
      Errores;
    if Todo or Tiene('--costos') then
      Costos;
    if Todo or Tiene('--usuarios') then
      Usuarios(ValorDe('--usuarios', 4));
    if Todo or Tiene('--ritmo') then
      Ritmo;
  except
    on E: Exception do
      Writeln(E.ClassName + ': ' + E.Message);
  end;
end.
