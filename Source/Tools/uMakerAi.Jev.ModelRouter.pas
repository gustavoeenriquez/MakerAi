// MIT License
//
// MakerAI - Jev: enrutador de modelos LLM con el modelo Jev (TypeSafe AI)
//
// Nombre: Gustavo Enriquez
// - Email: gustavoeenriquez@gmail.com
// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.Jev.ModelRouter;

// -----------------------------------------------------------------------------
// TAiJevModelRouter: elige, para cada peticion, el modelo mas barato que
// alcanza. Jev NO elige el modelo (no conoce precios ni capacidades): describe
// la peticion en una llamada con tres preguntas, y el CODIGO decide con reglas
// que se pueden leer y cambiar:
//
//   tarea       Choice  conversacion / redaccion / codigo / analisis /
//                       matematicas / datos
//   dificultad  Score   0 trivial .. 3 experta
//   sensible    Noul    decisiones legales, medicas, tributarias o
//                       financieras con consecuencias reales
//
//   nivel = round(dificultad)
//         + 1 si la confianza en la dificultad es < 0.5 (ante la duda, subir)
//         >= CodeMinLevel si la tarea es codigo, analisis o matematicas
//         >= SensitiveMinLevel si sensible > SensitiveThreshold
//         =  3 si ni siquiera se sabe que tarea es (confianza < 0.4)
//
// El modelo es el Tier mas barato (Cost) cuyo MaxLevel alcanza el nivel.
//
//   Router.Tiers.AddTier('rapido',   'Groq',     'openai/gpt-oss-20b',   0, 0.3);
//   Router.Tiers.AddTier('estandar', 'DeepSeek', 'deepseek-v4-flash',    1, 0.3);
//   Router.Tiers.AddTier('experto',  'Claude',   'claude-opus-5',        3, 15);
//   Respuesta := Router.Ask(Conexion, Pregunta);   // enruta, aplica y ejecuta
//
// Cambiar de proveedor en TAiChatConnection recrea el chat interno y el
// historial se pierde. Con MigrateHistory (defecto) se copian los mensajes de
// texto user/assistant al chat nuevo; los de tool calls NO se copian (su
// formato y sus ids son propios de cada proveedor). Cambiar solo de modelo
// dentro del mismo proveedor conserva todo. El SystemPrompt vive en la
// conexion y sobrevive en ambos casos. Los Params NO: el driver nuevo trae los
// suyos (su ApiKey '@<PROVEEDOR>_API_KEY' incluida); lo que deba valer para
// todos los modelos (Asynchronous=False, Max_Tokens, ...) va en
// ConnectionParams, que se aplica en cada Apply; lo propio de un modelo va en
// el Params de su tier (se aplica despues). Ejemplo real: claude-opus-5 con el
// razonamiento por defecto supero el timeout HTTP (~300 s) en una pregunta de
// diseno; con ThinkingLevel=tlLow respondio en 91 s.
//
// Calibrado contra jev-1.13.0 (sep 28/2026) sobre 16 peticiones de dificultad
// conocida: 13 exactas, 2 un nivel arriba (gastan de mas), 1 un nivel abajo.
// -----------------------------------------------------------------------------

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Math, System.Generics.Collections,
  uMakerAi.Jev, uMakerAi.Chat.Messages, uMakerAi.Chat.AiConnection;

type
  TAiModelRoute = record
    TierName: string;
    DriverName: string;
    Model: string;
    Level: Integer;          // 0 trivial .. 3 experto
    Task: string;
    TaskConfidence: Double;
    Difficulty: Double;      // valor esperado 0..3
    DifficultyConfidence: Double;
    Sensitive: Double;
    Reason: string;          // por que ese nivel, legible
  end;

  TAiJevModelTier = class(TCollectionItem)
  private
    FName: string;
    FDriverName: string;
    FModel: string;
    FMaxLevel: Integer;
    FCost: Double;
    FParams: TStrings;
    procedure SetParams(const Value: TStrings);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Name: string read FName write FName;
    property DriverName: string read FDriverName write FDriverName;
    property Model: string read FModel write FModel;
    // Nivel mas alto (0..3) que se le confia a este modelo
    property MaxLevel: Integer read FMaxLevel write FMaxLevel;
    // Costo relativo (p.ej. US$ por millon de tokens de salida); decide entre empatados
    property Cost: Double read FCost write FCost;
    // 'nombre=valor' propios de este modelo (p.ej. ThinkingLevel, Max_Tokens);
    // se aplican a la conexion despues de ConnectionParams
    property Params: TStrings read FParams write SetParams;
  end;

  TAiJevModelTiers = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TAiJevModelTier;
  public
    constructor Create(AOwner: TPersistent);
    function AddTier(const AName, ADriverName, AModel: string; AMaxLevel: Integer;
      ACost: Double): TAiJevModelTier;
    // El mas barato con MaxLevel >= ALevel; si ninguno alcanza, el de MaxLevel mas alto
    function Pick(ALevel: Integer): TAiJevModelTier;
    function FindTier(const AName: string): TAiJevModelTier;
    property Items[Index: Integer]: TAiJevModelTier read GetItem; default;
  end;

  TAiJevModelRouteEvent = procedure(Sender: TObject; const APrompt: string;
    var ARoute: TAiModelRoute) of object;

  TAiJevModelRouter = class(TComponent)
  private
    FUsage: TAiJevUsageMeter;
    FPricePerMillionInput: Double;
    FPricePerMillionOutput: Double;
    FOnUsage: TAiJevUsageEvent;
    FJev: TAiJev;
    FOwnJev: TAiJev;
    FApiKey: string;
    FModel: string;
    FUrl: string; // '' = TypeSafe (JEV_DEFAULT_URL)
    FTiers: TAiJevModelTiers;
    FSensitiveThreshold: Double;
    FSensitiveMinLevel: Integer;
    FCodeMinLevel: Integer;
    FBumpOnDoubt: Boolean;
    FMigrateHistory: Boolean;
    FConnectionParams: TStrings;
    FLastRoute: TAiModelRoute;
    FOnRoute: TAiJevModelRouteEvent;
    procedure SetJev(const Value: TAiJev);
    procedure SetTiers(const Value: TAiJevModelTiers);
    procedure SetConnectionParams(const Value: TStrings);
    function ActiveJev: TAiJev;
    function GetUsage: TAiJevUsage;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Consumo de Jev acumulado desde Create o ResetUsage (seguro entre hilos)
    property Usage: TAiJevUsage read GetUsage;
    procedure ResetUsage;
    // Solo decide (una llamada a Jev); no toca ninguna conexion
    function Route(const APrompt: string): TAiModelRoute;
    // Pone DriverName/Model en la conexion; migra el historial si cambia de proveedor
    procedure Apply(AConnection: TAiChatConnection; const ARoute: TAiModelRoute);
    // Route + Apply + AddMessageAndRun
    function Ask(AConnection: TAiChatConnection; const APrompt: string): string;
    property LastRoute: TAiModelRoute read FLastRoute;
  published
    // Opcional: TAiJev externo (compartido o doble de pruebas)
    property Jev: TAiJev read FJev write SetJev;
    property ApiKey: string read FApiKey write FApiKey;
    property Model: string read FModel write FModel;
    // Servidor System One: '' = TypeSafe; 'http://localhost:11434/v1/' = Ollama
    // (con Model := 'nimble', 'clef-flash'...). Ignorada si Jev esta asignado
    property Url: string read FUrl write FUrl;
    property Tiers: TAiJevModelTiers read FTiers write SetTiers;
    property SensitiveThreshold: Double read FSensitiveThreshold write FSensitiveThreshold;
    // Nivel minimo para lo sensible (2 = nunca al modelo mas barato)
    property SensitiveMinLevel: Integer read FSensitiveMinLevel write FSensitiveMinLevel default 2;
    // Nivel minimo para codigo, analisis y matematicas
    property CodeMinLevel: Integer read FCodeMinLevel write FCodeMinLevel default 1;
    // Subir un nivel si Jev duda de la dificultad
    property BumpOnDoubt: Boolean read FBumpOnDoubt write FBumpOnDoubt default True;
    // Copiar los mensajes de texto al cambiar de proveedor
    property MigrateHistory: Boolean read FMigrateHistory write FMigrateHistory default True;
    // 'nombre=valor' que se aplican a AConnection.Params en cada Apply
    // (el cambio de proveedor recarga los Params por defecto del driver)
    property ConnectionParams: TStrings read FConnectionParams write SetConnectionParams;
    property OnRoute: TAiJevModelRouteEvent read FOnRoute write FOnRoute;
    // Precios para CostUSD de Usage/OnUsage (US$ por millon de tokens; hoy la
    // salida no se cobra)
    property PricePerMillionInput: Double read FPricePerMillionInput write FPricePerMillionInput;
    property PricePerMillionOutput: Double read FPricePerMillionOutput write FPricePerMillionOutput;
    // Una vez por operacion con el consumo de esa operacion. Sincrono, en el hilo
    // que la ejecuto (en un servidor: el de la peticion, para cobrarle al cliente)
    property OnUsage: TAiJevUsageEvent read FOnUsage write FOnUsage;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiJevModelRouter]);
end;

function Inv(AValue: Double): string;
begin
  Result := FormatFloat('0.00', AValue, TFormatSettings.Invariant);
end;

{ TAiJevModelTier }

constructor TAiJevModelTier.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FParams := TStringList.Create;
end;

destructor TAiJevModelTier.Destroy;
begin
  FParams.Free;
  inherited;
end;

procedure TAiJevModelTier.SetParams(const Value: TStrings);
begin
  FParams.Assign(Value);
end;

function TAiJevModelTier.GetDisplayName: string;
begin
  if FName <> '' then
    Result := Format('%s (%s/%s, <= %d)', [FName, FDriverName, FModel, FMaxLevel])
  else
    Result := inherited GetDisplayName;
end;

procedure TAiJevModelTier.Assign(Source: TPersistent);
begin
  if Source is TAiJevModelTier then
  begin
    FName := TAiJevModelTier(Source).FName;
    FDriverName := TAiJevModelTier(Source).FDriverName;
    FModel := TAiJevModelTier(Source).FModel;
    FMaxLevel := TAiJevModelTier(Source).FMaxLevel;
    FCost := TAiJevModelTier(Source).FCost;
    FParams.Assign(TAiJevModelTier(Source).FParams);
  end
  else
    inherited;
end;

{ TAiJevModelTiers }

constructor TAiJevModelTiers.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TAiJevModelTier);
end;

function TAiJevModelTiers.GetItem(Index: Integer): TAiJevModelTier;
begin
  Result := TAiJevModelTier(inherited GetItem(Index));
end;

function TAiJevModelTiers.AddTier(const AName, ADriverName, AModel: string; AMaxLevel: Integer;
  ACost: Double): TAiJevModelTier;
begin
  Result := TAiJevModelTier(Add);
  Result.Name := AName;
  Result.DriverName := ADriverName;
  Result.Model := AModel;
  Result.MaxLevel := AMaxLevel;
  Result.Cost := ACost;
end;

function TAiJevModelTiers.FindTier(const AName: string): TAiJevModelTier;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if SameText(Items[i].Name, AName) then
      Exit(Items[i]);
  Result := nil;
end;

function TAiJevModelTiers.Pick(ALevel: Integer): TAiJevModelTier;
var
  i: Integer;
  T: TAiJevModelTier;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    T := Items[i];
    if (T.MaxLevel >= ALevel) and ((Result = nil) or (T.Cost < Result.Cost)) then
      Result := T;
  end;
  if Result <> nil then
    Exit;
  // Ninguno alcanza: el mas capaz disponible
  for i := 0 to Count - 1 do
    if (Result = nil) or (Items[i].MaxLevel > Result.MaxLevel) then
      Result := Items[i];
end;

{ TAiJevModelRouter }

constructor TAiJevModelRouter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUsage := TAiJevUsageMeter.Create;
  FPricePerMillionInput := JEV_PRICE_PER_MILLION_INPUT;
  FApiKey := '@TYPESAFE_API_KEY';
  FModel := 'jev-1.13.0';
  FTiers := TAiJevModelTiers.Create(Self);
  FSensitiveThreshold := 0.6;
  FSensitiveMinLevel := 2;
  FCodeMinLevel := 1;
  FBumpOnDoubt := True;
  FMigrateHistory := True;
  FConnectionParams := TStringList.Create;
end;

destructor TAiJevModelRouter.Destroy;
begin
  FTiers.Free;
  FConnectionParams.Free;
  FOwnJev.Free;
  FUsage.Free;
  inherited;
end;

procedure TAiJevModelRouter.SetJev(const Value: TAiJev);
begin
  if FJev = Value then
    Exit;
  if Assigned(FJev) then
    FJev.RemoveFreeNotification(Self);
  FJev := Value;
  if Assigned(FJev) then
    FJev.FreeNotification(Self);
end;

procedure TAiJevModelRouter.SetTiers(const Value: TAiJevModelTiers);
begin
  FTiers.Assign(Value);
end;

procedure TAiJevModelRouter.SetConnectionParams(const Value: TStrings);
begin
  FConnectionParams.Assign(Value);
end;

procedure TAiJevModelRouter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FJev) then
    FJev := nil;
end;

function TAiJevModelRouter.ActiveJev: TAiJev;
begin
  if Assigned(FJev) then
    Exit(FJev);
  if not Assigned(FOwnJev) then
    FOwnJev := TAiJev.Create(nil);
  FOwnJev.ApiKey := FApiKey;
  FOwnJev.Model := FModel;
  FOwnJev.Url := JevUrlOrDefault(FUrl);
  Result := FOwnJev;
end;

function TAiJevModelRouter.Route(const APrompt: string): TAiModelRoute;
var
  Q: TAiJevQuestions;
  State: TJSONObject;
  R: TAiJevResult;
  Tier: TAiJevModelTier;
  Reasons: TStringList;
begin
  if FTiers.Count = 0 then
    raise EAiJevError.Create('JevModelRouter: no hay Tiers definidos');

  Q := TAiJevQuestions.Create(nil);
  Reasons := TStringList.Create;
  try
    // Mismas preguntas que el prototipo calibrado (router.py)
    Q.AddChoice('tarea', '¿Qué tipo de tarea pide `pregunta`?', [
      'conversacion=Saludo, charla o pregunta de conocimiento general con respuesta corta',
      'redaccion=Escribir, resumir, traducir o corregir un texto',
      'codigo=Escribir, depurar, revisar o explicar código de programación',
      'analisis=Razonar en varios pasos, comparar opciones, planear o decidir',
      'matematicas=Resolver cálculos, problemas numéricos o demostraciones',
      'datos=Extraer, clasificar o transformar datos estructurados']);
    Q.AddScore('dificultad', '¿Qué tan difícil es responder bien `pregunta`?', [
      'Trivial: la respuesta es un dato o una frase',
      'Estándar: un profesional la responde sin investigar',
      'Exigente: requiere varios pasos de razonamiento o contexto largo',
      'Experta: diseño complejo, casos límite o alto costo de equivocarse']);
    Q.AddNoul('sensible', '¿`pregunta` involucra decisiones legales, médicas, tributarias o ' +
      'financieras donde un error tendría consecuencias reales?');

    State := TJSONObject.Create;
    try
      State.AddPair('pregunta', APrompt);
      R := ActiveJev.Ask(State, Q);
    finally
      State.Free;
    end;
    try
      JevReportResult(Self, R, FUsage, JevAdapterInputPrice(FJev, FUrl, FPricePerMillionInput), FPricePerMillionOutput, FOnUsage);
      Result.Task := R['tarea'].Choice;
      Result.TaskConfidence := R['tarea'].Confidence;
      Result.Difficulty := R['dificultad'].Score;
      Result.DifficultyConfidence := R['dificultad'].Confidence;
      Result.Sensitive := R['sensible'].Noul;
    finally
      R.Free;
    end;

    // Reglas en codigo (mismas del prototipo), con el motivo de cada ajuste
    Result.Level := EnsureRange(Round(Result.Difficulty), 0, 3);
    Reasons.Add('dificultad ' + Inv(Result.Difficulty));
    if FBumpOnDoubt and (Result.DifficultyConfidence < 0.5) then
    begin
      Inc(Result.Level);
      Reasons.Add('duda ' + Inv(Result.DifficultyConfidence) + ' +1');
    end;
    if ((Result.Task = 'codigo') or (Result.Task = 'analisis') or (Result.Task = 'matematicas')) and
       (Result.Level < FCodeMinLevel) then
    begin
      Result.Level := FCodeMinLevel;
      Reasons.Add(Result.Task + ' >= ' + IntToStr(FCodeMinLevel));
    end;
    if (Result.Sensitive > FSensitiveThreshold) and (Result.Level < FSensitiveMinLevel) then
    begin
      Result.Level := FSensitiveMinLevel;
      Reasons.Add('sensible ' + Inv(Result.Sensitive) + ' >= ' + IntToStr(FSensitiveMinLevel));
    end;
    if Result.TaskConfidence < 0.4 then
    begin
      Result.Level := 3;
      Reasons.Add('tarea incierta ' + Inv(Result.TaskConfidence) + ' -> 3');
    end;
    Result.Level := EnsureRange(Result.Level, 0, 3);

    Tier := FTiers.Pick(Result.Level);
    Result.TierName := Tier.Name;
    Result.DriverName := Tier.DriverName;
    Result.Model := Tier.Model;
    Reasons.Delimiter := ',';
    Reasons.StrictDelimiter := True;
    Result.Reason := Reasons.DelimitedText;
  finally
    Reasons.Free;
    Q.Free;
  end;

  if Assigned(FOnRoute) then
    FOnRoute(Self, APrompt, Result);
  FLastRoute := Result;
end;

procedure TAiJevModelRouter.Apply(AConnection: TAiChatConnection; const ARoute: TAiModelRoute);
var
  History: TList<TPair<string, string>>;
  M: TAiChatMessage;
  P: TPair<string, string>;

  procedure ApplyList(AList: TStrings);
  var
    i: Integer;
  begin
    for i := 0 to AList.Count - 1 do
      if AList.Names[i] <> '' then
        AConnection.Params.Values[AList.Names[i]] := AList.ValueFromIndex[i];
  end;

  procedure ApplyParams;
  var
    Tier: TAiJevModelTier;
  begin
    ApplyList(FConnectionParams);
    Tier := FTiers.FindTier(ARoute.TierName);
    if Assigned(Tier) then
      ApplyList(Tier.Params); // lo propio del modelo gana sobre lo global
  end;

begin
  if not Assigned(AConnection) then
    Exit;

  if SameText(AConnection.DriverName, ARoute.DriverName) then
  begin
    // Mismo proveedor: cambiar el modelo conserva el chat y su historial
    if AConnection.Model <> ARoute.Model then
      AConnection.Model := ARoute.Model;
    ApplyParams;
    Exit;
  end;

  History := TList<TPair<string, string>>.Create;
  try
    // Cambiar de proveedor recrea el chat: se guardan antes los mensajes de texto
    if FMigrateHistory and Assigned(AConnection.Messages) then
      for M in AConnection.Messages do
        if ((M.Role = 'user') or (M.Role = 'assistant')) and (M.Prompt <> '') and (M.Tool_calls = '') then
          History.Add(TPair<string, string>.Create(M.Role, M.Prompt));

    AConnection.DriverName := ARoute.DriverName;
    AConnection.Model := ARoute.Model;
    // Despues del modelo: asignar Model recarga sus defaults (p.ej. Max_Tokens)
    ApplyParams;

    for P in History do
      AConnection.AddMessage(P.Value, P.Key);
  finally
    History.Free;
  end;
end;

function TAiJevModelRouter.Ask(AConnection: TAiChatConnection; const APrompt: string): string;
begin
  Apply(AConnection, Route(APrompt));
  Result := AConnection.AddMessageAndRun(APrompt, 'user', []);
end;

function TAiJevModelRouter.GetUsage: TAiJevUsage;
begin
  Result := FUsage.Snapshot(JevAdapterInputPrice(FJev, FUrl, FPricePerMillionInput), FPricePerMillionOutput);
end;

procedure TAiJevModelRouter.ResetUsage;
begin
  FUsage.Reset;
end;

end.
