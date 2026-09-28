program JevModelRouterDemo;

// =============================================================================
// DEMO 088 - Enrutador de modelos con Jev: el modelo mas barato que alcanza
// =============================================================================
// TAiJevModelRouter describe cada peticion con Jev (tarea, dificultad,
// sensibilidad) y elige en codigo el tier mas barato que alcanza:
//
//   nivel 0  Groq     openai/gpt-oss-20b     trivial
//   nivel 1  DeepSeek deepseek-v4-flash      estandar
//   nivel 2  Claude   claude-sonnet-5        exigente / sensible
//   nivel 3  Claude   claude-opus-5          experto
//
//   A. Peticiones sueltas: cada una la responde el modelo elegido, y
//      TAiJevEvalScorer mide si la respuesta es correcta y util. Es la
//      prueba de calidad: que el modelo barato conteste bien lo que le toca.
//   B. Una conversacion que empieza trivial (Groq) y escala a experta
//      (Claude Opus): el historial se migra entre proveedores y el modelo
//      nuevo debe recordar lo que se dijo en el primer turno.
//
// Requiere TYPESAFE_API_KEY, GROQ_API_KEY, DEEPSEEK_API_KEY y CLAUDE_API_KEY.
// Las respuestas se limitan a 1200 tokens para acotar el costo.
// =============================================================================

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.Diagnostics,
  System.StrUtils,
  System.Net.HttpClient,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations,   // registra los drivers (Groq, DeepSeek, Claude...)
  uMakerAi.Jev in '..\..\Source\Tools\uMakerAi.Jev.pas',
  uMakerAi.Jev.Evals in '..\..\Source\Tools\uMakerAi.Jev.Evals.pas',
  uMakerAi.Jev.ModelRouter in '..\..\Source\Tools\uMakerAi.Jev.ModelRouter.pas';

const
  PETICIONES: array[0..7] of string = (
    'Hola, buenos dias',
    'Traduce al ingles: la reunion se aplaza para el lunes',
    'Escribe una funcion en Delphi que invierta un string',
    'Explicame la diferencia entre una clase abstracta y una interfaz en Delphi',
    'Tengo un access violation al liberar un TStringList dentro de un finally, que puede ser?',
    'Puedo deducir en renta los intereses de un credito de vivienda si soy independiente en Colombia?',
    'Compara PostgreSQL y SQL Server para un ERP multiempresa y recomienda uno con argumentos',
    'Disena la arquitectura multi-tenant de un ERP contable con PostgreSQL, sincronizacion offline y auditoria');

  CRITERIO_CALIDAD = 'It correctly and usefully answers the user request in `input`';

type
  // Los eventos del framework son 'of object': el handler vive en una clase
  TErrores = class
  public
    Ultimo: string;
    procedure OnError(Sender: TObject; const ErrorMsg: string; Exception: Exception;
      const AResponse: IHTTPResponse);
  end;

var
  Errores: TErrores;

procedure TErrores.OnError(Sender: TObject; const ErrorMsg: string; Exception: Exception;
  const AResponse: IHTTPResponse);
begin
  Ultimo := ErrorMsg;
end;

function Corto(const S: string; N: Integer): string;
begin
  Result := StringReplace(StringReplace(Trim(S), #13, ' ', [rfReplaceAll]), #10, ' ', [rfReplaceAll]);
  if Length(Result) > N then
    Result := Copy(Result, 1, N) + '...';
end;

procedure Configurar(ARouter: TAiJevModelRouter);
var
  T: TAiJevModelTier;
begin
  // Groq retiro los llama-3.x (model_not_found, sep 2026): gpt-oss-20b es su modelo rapido
  ARouter.Tiers.AddTier('rapido',   'Groq',     'openai/gpt-oss-20b', 0, 0.3);
  ARouter.Tiers.AddTier('estandar', 'DeepSeek', 'deepseek-v4-flash',  1, 0.4);
  // Claude: razonamiento bajo y sin busqueda web. Con los defaults, Opus supero
  // el timeout HTTP (~300 s) en la pregunta de arquitectura; asi respondio en 91 s.
  T := ARouter.Tiers.AddTier('exigente', 'Claude', 'claude-sonnet-5', 2, 15);
  T.Params.Add('ThinkingLevel=tlLow');
  T.Params.Add('SessionCaps=[cap_Image, cap_Pdf]');
  T := ARouter.Tiers.AddTier('experto', 'Claude', 'claude-opus-5', 3, 25);
  T.Params.Add('ThinkingLevel=tlLow');
  T.Params.Add('SessionCaps=[cap_Image, cap_Pdf]');
  T.Params.Add('Max_Tokens=3000');
  // El cambio de proveedor recarga los Params del driver: esto vale para todos
  ARouter.ConnectionParams.Add('Asynchronous=False');
  ARouter.ConnectionParams.Add('Max_Tokens=1200');
end;

procedure BloquePeticiones(ARouter: TAiJevModelRouter; AScorer: TAiJevEvalScorer);
var
  P, Resp: string;
  Conn: TAiChatConnection;
  R: TAiModelRoute;
  SW: TStopwatch;
  Q: Double;
  Buenas: Integer;
begin
  Writeln('--- A. Cada peticion al modelo mas barato que alcanza ---');
  Writeln;
  Buenas := 0;
  for P in PETICIONES do
  begin
    Conn := TAiChatConnection.Create(nil);
    try
      Conn.OnError := Errores.OnError;
      Errores.Ultimo := '';
      R := ARouter.Route(P);
      ARouter.Apply(Conn, R);
      SW := TStopwatch.StartNew;
      Resp := Conn.AddMessageAndRun(P, 'user', []);
      SW.Stop;
      Q := AScorer.Score(CRITERIO_CALIDAD, P, Resp);
      if Q >= 0.7 then
        Inc(Buenas);
      Writeln(Format('[nivel %d] %-9s %-22s %5d ms  calidad %.2f', [R.Level, R.TierName, R.Model,
        SW.ElapsedMilliseconds, Q]));
      Writeln('  pregunta: ', Corto(P, 90));
      Writeln('  motivo:   ', R.Reason);
      Writeln('  respuesta: ', Corto(Resp, 110));
      if Errores.Ultimo <> '' then
        Writeln('  ERROR: ', Corto(Errores.Ultimo, 200));
      Writeln;
    finally
      Conn.Free;
    end;
  end;
  Writeln(Format('Respuestas utiles (calidad >= 0.70): %d de %d', [Buenas, Length(PETICIONES)]));
  Writeln;
end;

procedure BloqueConversacion(ARouter: TAiJevModelRouter; AScorer: TAiJevEvalScorer);
const
  TURNO1 = 'Hola, me llamo Gustavo y tengo una ferreteria en Bogota que vende tornillos al por mayor.';
  TURNO2 = 'Disena un plan de expansion a tres ciudades para mi negocio, teniendo en cuenta lo que te conte.';
var
  Conn: TAiChatConnection;
  Resp: string;
  Q: Double;
begin
  Writeln('--- B. Una conversacion que escala de proveedor ---');
  Writeln;
  Conn := TAiChatConnection.Create(nil);
  try
    Resp := ARouter.Ask(Conn, TURNO1);
    Writeln(Format('Turno 1 -> %s / %s (nivel %d)', [ARouter.LastRoute.DriverName,
      ARouter.LastRoute.Model, ARouter.LastRoute.Level]));
    Writeln('  ', Corto(Resp, 110));

    Resp := ARouter.Ask(Conn, TURNO2);
    Writeln(Format('Turno 2 -> %s / %s (nivel %d), mensajes en la conversacion: %d',
      [ARouter.LastRoute.DriverName, ARouter.LastRoute.Model, ARouter.LastRoute.Level,
       Conn.Messages.Count]));
    Writeln('  ', Corto(Resp, 160));

    // El modelo nuevo solo puede saber esto si el historial se migro
    Q := AScorer.Score('It builds on the fact that the user owns a hardware store in Bogota ' +
      'selling screws wholesale', TURNO2, Resp);
    Writeln(Format('  Recuerda el negocio del turno 1: %s (Jev %.2f; menciona ferreteria/tornillos: %s)',
      [IfThen(Q >= 0.7, 'si', 'NO'), Q,
       IfThen(ContainsText(Resp, 'ferreter') or ContainsText(Resp, 'tornillo'), 'si', 'no')]));
  finally
    Conn.Free;
  end;
  Writeln;
end;

var
  Router: TAiJevModelRouter;
  Scorer: TAiJevEvalScorer;
begin
  try
    if GetEnvironmentVariable('TYPESAFE_API_KEY') = '' then
    begin
      Writeln('Falta la variable de entorno TYPESAFE_API_KEY (https://console.typesafe.ai/keys).');
      ExitCode := 2;
      Exit;
    end;
    Errores := TErrores.Create;
    Router := TAiJevModelRouter.Create(nil);
    Scorer := TAiJevEvalScorer.Create(nil);
    try
      Configurar(Router);
      BloquePeticiones(Router, Scorer);
      BloqueConversacion(Router, Scorer);
    finally
      Router.Free;
      Scorer.Free;
      Errores.Free;
    end;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ExitCode := 2;
    end;
  end;
end.
