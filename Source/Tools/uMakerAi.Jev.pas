// MIT License
//
// MakerAI - Jev: preguntas tipadas al modelo Jev de TypeSafe AI (System One)
//
// Nombre: Gustavo Enriquez
// - Email: gustavoeenriquez@gmail.com
// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.Jev;

// -----------------------------------------------------------------------------
// TAiJev: cliente del modelo Jev (TypeSafe AI, https://docs.typesafe.ai).
//
// Jev NO es un LLM de chat: no genera texto. Recibe un "state" (texto o JSON)
// y un mapa de preguntas tipadas, y devuelve por cada una probabilidades
// calibradas y una confianza. Sirve para decisiones rapidas y baratas que hoy
// se le piden a un LLM completo: enrutar una consulta al agente correcto,
// clasificar, filtrar relevancia, aprobar o no una accion.
//
// Tres tipos de pregunta:
//   jqChoice  elige una opcion de un conjunto (max. 255). Criteria: una linea
//             por opcion, 'clave=descripcion' (la descripcion es opcional).
//   jqScore   califica contra niveles ordenados (2 a 10). Criteria: una linea
//             por nivel, del mas bajo al mas alto.
//   jqNoul    si/no; devuelve la probabilidad de "si". Criteria opcional:
//             'true=...' y 'false=...'.
//
// Varias preguntas viajan en UNA sola llamada (se cobra el state una vez):
// preferir un TAiJevQuestions con todo lo que se necesita decidir a llamadas
// sueltas.
//
// Lo que el modelo NO hace bien (docs: model-jaggedness): aritmetica, contar,
// comparar fechas y generar texto. Eso va en codigo; a Jev solo el juicio.
//
// Umbrales: la confianza de una misma entrada varia unos puntos entre llamadas.
// Fijar Model a una version concreta (por defecto 'jev-1.13.0', no
// 'jev-latest') antes de calibrar umbrales, y dejarles margen.
//
// DoPost es virtual: la suite de regresion lo sustituye para probar sin red.
//
// Otros servidores System One (oct 2026): Ollama 0.35+ expone el mismo
// contrato en http://localhost:11434/v1/systemone con modelos de decision
// locales (nimble, tev1, clef, clef-flash). Basta con Url + Model:
//   Jev.Url := 'http://localhost:11434/v1/'; Jev.Model := 'nimble';
// Diferencias: Choice y Score admiten de 2 a 26 opciones (el servidor rechaza
// mas con HTTP 400), requests de hasta 64 KiB, sin API key. Clef y Clef Flash
// (Ollama 0.35.1+) leen IMAGENES: Ask(State, [Imagen1, ...], Preguntas).
// El precio por defecto (JEV_PRICE_PER_MILLION_INPUT) solo se cobra cuando
// Url apunta a TypeSafe; un precio fijado a mano se respeta siempre.
// -----------------------------------------------------------------------------

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections, System.SyncObjs,
  uMakerAi.Core;

type
  EAiJevError = class(Exception)
  private
    FStatusCode: Integer;
  public
    constructor Create(const AMsg: string; AStatusCode: Integer = 0);
    // Codigo HTTP de la respuesta; 0 si el error es local (validacion, parseo)
    property StatusCode: Integer read FStatusCode;
  end;

  TAiJevQuestionKind = (jqChoice, jqScore, jqNoul);

  TAiJevQuestion = class(TCollectionItem)
  private
    FName: string;
    FKind: TAiJevQuestionKind;
    FInstructions: string;
    FCriteria: TStrings;
    procedure SetCriteria(const Value: TStrings);
    function CriteriaLines: TArray<string>;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    // Lanza EAiJevError si la pregunta no cumple las reglas de la API
    procedure Validate;
    function ToJSON: TJSONObject;
  published
    // Id de la pregunta: la respuesta vuelve bajo este mismo nombre
    property Name: string read FName write FName;
    property Kind: TAiJevQuestionKind read FKind write FKind default jqChoice;
    // La pregunta. Puede referirse a campos del state entre backticks: `consulta`
    property Instructions: string read FInstructions write FInstructions;
    property Criteria: TStrings read FCriteria write SetCriteria;
  end;

  TAiJevQuestions = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TAiJevQuestion;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TAiJevQuestion;
    // AOptions: 'clave' o 'clave=descripcion'
    function AddChoice(const AName, AInstructions: string; const AOptions: array of string): TAiJevQuestion;
    // ALevels: del mas bajo al mas alto
    function AddScore(const AName, AInstructions: string; const ALevels: array of string): TAiJevQuestion;
    function AddNoul(const AName, AInstructions: string; const AWhenTrue: string = '';
      const AWhenFalse: string = ''): TAiJevQuestion;
    function Find(const AName: string): TAiJevQuestion;
    procedure Validate;
    property Items[Index: Integer]: TAiJevQuestion read GetItem; default;
  end;

  TAiJevAnswer = class
  private
    FName: string;
    FKind: TAiJevQuestionKind;
    FChoice: string;
    FScore: Double;
    FNoul: Double;
    FConfidence: Double;
    FProbabilities: TDictionary<string, Double>;
  public
    constructor Create;
    destructor Destroy; override;
    // Probabilidad de una opcion (Choice) o nivel (Score, clave '0','1',...); 0 si no existe
    function Probability(const AOption: string): Double;
    // Opciones ordenadas de mayor a menor probabilidad
    function Ranked: TArray<TPair<string, Double>>;
    // Las ACount opciones mas probables (sugerencias para revision humana)
    function Top(ACount: Integer): TArray<string>;

    property Name: string read FName;
    property Kind: TAiJevQuestionKind read FKind;
    // jqChoice: opcion con mayor probabilidad
    property Choice: string read FChoice;
    // jqScore: valor esperado entre niveles (0 = primer nivel); puede caer entre dos
    property Score: Double read FScore;
    // jqNoul: probabilidad de "si" (0..1)
    property Noul: Double read FNoul;
    // jqChoice / jqScore: certeza derivada de la distribucion (0..1). La API no la da para Noul.
    property Confidence: Double read FConfidence;
    property Probabilities: TDictionary<string, Double> read FProbabilities;
  end;

  TAiJevResult = class
  private
    FModel: string;
    FInputTokens: Integer;
    FOutputTokens: Integer;
    FAnswers: TObjectDictionary<string, TAiJevAnswer>;
    function GetAnswer(const AName: string): TAiJevAnswer;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    // Parsea el cuerpo de respuesta de /v1/systemone
    class function FromJSON(const AJson: string): TAiJevResult;
    function TryGetAnswer(const AName: string; out AAnswer: TAiJevAnswer): Boolean;
    // Lanza EAiJevError si no hay respuesta con ese nombre
    property Answers[const AName: string]: TAiJevAnswer read GetAnswer; default;
    property Count: Integer read GetCount;
    // Version concreta que respondio (p.ej. 'jev-1.13.0' aunque se pidiera un alias)
    property Model: string read FModel;
    property InputTokens: Integer read FInputTokens;
    property OutputTokens: Integer read FOutputTokens;
  end;

  // Consumo de Jev: peticiones, tokens y costo. Precio vigente (sep 2026):
  // US$0.042 por millon de tokens de entrada; la salida no se cobra.
  TAiJevUsage = record
    Requests: Int64;
    InputTokens: Int64;
    OutputTokens: Int64;
    CostUSD: Double;
  end;

  // Se dispara SINCRONO en el hilo que ejecuto la operacion (no por TThread.Queue):
  // en un servidor ese es el hilo de la peticion, asi que el integrador sabe a
  // que cliente cargarle el consumo.
  TAiJevUsageEvent = procedure(Sender: TObject; const AUsage: TAiJevUsage) of object;

  // Acumulador seguro entre hilos (los adaptadores llaman a Jev en paralelo)
  TAiJevUsageMeter = class
  private
    FRequests, FInputTokens, FOutputTokens: Int64;
  public
    procedure Add(AResult: TAiJevResult); overload;
    procedure Add(const AUsage: TAiJevUsage); overload;
    procedure Reset;
    // Totales con el costo calculado a los precios dados (US$ por millon)
    function Snapshot(APricePerMillionInput, APricePerMillionOutput: Double): TAiJevUsage;
  end;

  TAiJev = class(TComponent)
  private
    FUsage: TAiJevUsageMeter;
    FPricePerMillionInput: Double;
    FPricePerMillionOutput: Double;
    FOnUsage: TAiJevUsageEvent;
    function GetUsage: TAiJevUsage;
  private
    FApiKey: string;
    FModel: string;
    FUrl: string;
    FQuestions: TAiJevQuestions;
    FMaxRetries: Integer;
    FRetryDelay: Integer;
    FTimeout: Integer;
    FLastError: string;
    FOnError: TAiErrorEvent;
    function GetApiKey: string;
    procedure SetQuestions(const Value: TAiJevQuestions);
  protected
    // Envia el cuerpo y devuelve el status HTTP. Virtual para sustituir la red en pruebas.
    function DoPost(const ABody: string; out AResponse: string): Integer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Cuerpo JSON del request (valida las preguntas y las imagenes). El
    // llamador libera el resultado.
    function BuildRequest(AState: TJSONValue; AQuestions: TAiJevQuestions): TJSONObject; overload;
    function BuildRequest(AState: TJSONValue; AQuestions: TAiJevQuestions;
      const AImages: array of TAiMediaFile): TJSONObject; overload;

    // AQuestions = nil usa la propiedad Questions. El llamador libera el resultado.
    // Lanza EAiJevError si la API responde con error (tras OnError y LastError).
    function Ask(const AState: string; AQuestions: TAiJevQuestions = nil): TAiJevResult; overload;
    // State estructurado (objeto o arreglo JSON). No toma posesion de AState.
    function Ask(AState: TJSONValue; AQuestions: TAiJevQuestions = nil): TAiJevResult; overload;
    // Con imagenes (PNG, JPEG o WebP, compartidas por todas las preguntas y
    // juzgadas junto con el state). Solo modelos que leen imagenes, como Clef
    // en Ollama 0.35.1+. No toma posesion de las imagenes.
    function Ask(const AState: string; const AImages: array of TAiMediaFile;
      AQuestions: TAiJevQuestions = nil): TAiJevResult; overload;
    function Ask(AState: TJSONValue; const AImages: array of TAiMediaFile;
      AQuestions: TAiJevQuestions = nil): TAiJevResult; overload;

    // Atajos para una sola pregunta
    function Choose(const AState, AInstructions: string; const AOptions: array of string;
      out AConfidence: Double): string;
    function Noul(const AState, AInstructions: string): Double;

    property LastError: string read FLastError;
    // Consumo acumulado de este componente desde que se creo o desde ResetUsage
    property Usage: TAiJevUsage read GetUsage;
    procedure ResetUsage;
  published
    // Precios para CostUSD (US$ por millon de tokens)
    property PricePerMillionInput: Double read FPricePerMillionInput write FPricePerMillionInput;
    property PricePerMillionOutput: Double read FPricePerMillionOutput write FPricePerMillionOutput;
    // Una vez por llamada exitosa a la API, con el consumo de esa llamada
    property OnUsage: TAiJevUsageEvent read FOnUsage write FOnUsage;
    // '@TYPESAFE_API_KEY' se resuelve con la variable de entorno
    property ApiKey: string read GetApiKey write FApiKey;
    property Model: string read FModel write FModel;
    property Url: string read FUrl write FUrl;
    // Preguntas definidas en diseno (se usan cuando Ask recibe AQuestions = nil)
    property Questions: TAiJevQuestions read FQuestions write SetQuestions;
    // Reintentos ante 429 / 503 / 529, con espera exponencial desde RetryDelay (ms)
    property MaxRetries: Integer read FMaxRetries write FMaxRetries default 2;
    property RetryDelay: Integer read FRetryDelay write FRetryDelay default 500;
    // Timeout de conexion y de respuesta (ms)
    property Timeout: Integer read FTimeout write FTimeout default 30000;
    property OnError: TAiErrorEvent read FOnError write FOnError;
  end;

const
  JEV_PRICE_PER_MILLION_INPUT = 0.042;
  JEV_DEFAULT_URL = 'https://api.typesafe.ai/v1/';

// True si la Url es la de TypeSafe ('' = la de TypeSafe por defecto)
function JevIsTypeSafeUrl(const AUrl: string): Boolean;
// '' -> JEV_DEFAULT_URL
function JevUrlOrDefault(const AUrl: string): string;
// Precio de entrada efectivo: el precio por defecto de TypeSafe no se cobra en
// otros servidores (Ollama local, etc.); un precio fijado a mano se respeta.
function JevEffectiveInputPrice(const AUrl: string; APrice: Double): Double;
// Igual para un adaptador: la Url que cuenta es la del Jev que realmente usa
// (el externo si esta asignado, si no la propia del adaptador).
function JevAdapterInputPrice(AExternalJev: TAiJev; const AUrl: string; APrice: Double): Double;

// Cierra una operacion de un adaptador: suma su consumo al total del adaptador
// y dispara OnUsage (sincrono, en el hilo del llamador) si hubo peticiones.
procedure JevReportOperation(ASender: TObject; AOperation, ATotal: TAiJevUsageMeter;
  APricePerMillionInput, APricePerMillionOutput: Double; AEvent: TAiJevUsageEvent);

// Igual para una operacion de una sola llamada: suma AResult al total del
// adaptador y dispara OnUsage con el consumo de esa llamada.
procedure JevReportResult(ASender: TObject; AResult: TAiJevResult; ATotal: TAiJevUsageMeter;
  APricePerMillionInput, APricePerMillionOutput: Double; AEvent: TAiJevUsageEvent);

procedure Register;

implementation

uses
  System.Math, System.Generics.Defaults, System.Net.HttpClient,
  System.Net.HttpClientComponent, System.Net.URLClient, System.NetEncoding,
  uMakerAi.Telemetry;

function JevIsTypeSafeUrl(const AUrl: string): Boolean;
begin
  Result := (Trim(AUrl) = '') or (Pos('typesafe.ai', LowerCase(AUrl)) > 0);
end;

function JevUrlOrDefault(const AUrl: string): string;
begin
  if Trim(AUrl) = '' then
    Result := JEV_DEFAULT_URL
  else
    Result := AUrl;
end;

function JevEffectiveInputPrice(const AUrl: string; APrice: Double): Double;
begin
  if SameValue(APrice, JEV_PRICE_PER_MILLION_INPUT) and not JevIsTypeSafeUrl(AUrl) then
    Result := 0
  else
    Result := APrice;
end;

function JevAdapterInputPrice(AExternalJev: TAiJev; const AUrl: string; APrice: Double): Double;
begin
  if Assigned(AExternalJev) then
    Result := JevEffectiveInputPrice(AExternalJev.Url, APrice)
  else
    Result := JevEffectiveInputPrice(JevUrlOrDefault(AUrl), APrice);
end;

// Formato real de la imagen por sus bytes iniciales: 'png', 'jpeg', 'webp' o ''
function JevImageKind(const AData: TBytes): string;
begin
  Result := '';
  if (Length(AData) >= 8) and (AData[0] = $89) and (AData[1] = $50) and
     (AData[2] = $4E) and (AData[3] = $47) then
    Result := 'png'
  else if (Length(AData) >= 3) and (AData[0] = $FF) and (AData[1] = $D8) and (AData[2] = $FF) then
    Result := 'jpeg'
  else if (Length(AData) >= 12) and (AData[0] = Ord('R')) and (AData[1] = Ord('I')) and
     (AData[2] = Ord('F')) and (AData[3] = Ord('F')) and (AData[8] = Ord('W')) and
     (AData[9] = Ord('E')) and (AData[10] = Ord('B')) and (AData[11] = Ord('P')) then
    Result := 'webp';
end;

const
  JEV_MAX_CHOICE_OPTIONS = 255;
  JEV_MIN_SCORE_LEVELS = 2;
  JEV_MAX_SCORE_LEVELS = 10;

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiJev]);
end;

{ TAiJevUsageMeter }

procedure TAiJevUsageMeter.Add(AResult: TAiJevResult);
begin
  if AResult = nil then Exit;
  TInterlocked.Increment(FRequests);
  TInterlocked.Add(FInputTokens, Int64(AResult.InputTokens));
  TInterlocked.Add(FOutputTokens, Int64(AResult.OutputTokens));
end;

procedure TAiJevUsageMeter.Add(const AUsage: TAiJevUsage);
begin
  TInterlocked.Add(FRequests, AUsage.Requests);
  TInterlocked.Add(FInputTokens, AUsage.InputTokens);
  TInterlocked.Add(FOutputTokens, AUsage.OutputTokens);
end;

procedure TAiJevUsageMeter.Reset;
begin
  TInterlocked.Exchange(FRequests, 0);
  TInterlocked.Exchange(FInputTokens, 0);
  TInterlocked.Exchange(FOutputTokens, 0);
end;

function TAiJevUsageMeter.Snapshot(APricePerMillionInput, APricePerMillionOutput: Double): TAiJevUsage;
begin
  Result.Requests := TInterlocked.Read(FRequests);
  Result.InputTokens := TInterlocked.Read(FInputTokens);
  Result.OutputTokens := TInterlocked.Read(FOutputTokens);
  Result.CostUSD := (Result.InputTokens * APricePerMillionInput +
    Result.OutputTokens * APricePerMillionOutput) / 1E6;
end;

procedure JevReportResult(ASender: TObject; AResult: TAiJevResult; ATotal: TAiJevUsageMeter;
  APricePerMillionInput, APricePerMillionOutput: Double; AEvent: TAiJevUsageEvent);
var
  Op: TAiJevUsageMeter;
begin
  Op := TAiJevUsageMeter.Create;
  try
    Op.Add(AResult);
    JevReportOperation(ASender, Op, ATotal, APricePerMillionInput, APricePerMillionOutput, AEvent);
  finally
    Op.Free;
  end;
end;

procedure JevReportOperation(ASender: TObject; AOperation, ATotal: TAiJevUsageMeter;
  APricePerMillionInput, APricePerMillionOutput: Double; AEvent: TAiJevUsageEvent);
var
  U: TAiJevUsage;
begin
  U := AOperation.Snapshot(APricePerMillionInput, APricePerMillionOutput);
  if U.Requests = 0 then
    Exit;
  ATotal.Add(U);
  if Assigned(AEvent) then
    AEvent(ASender, U);
end;

// 'clave=descripcion' -> clave, descripcion. Sin '=' la descripcion queda vacia.
procedure SplitOption(const ALine: string; out AKey, ADesc: string);
var
  P: Integer;
begin
  P := Pos('=', ALine);
  if P > 0 then
  begin
    AKey := Trim(Copy(ALine, 1, P - 1));
    ADesc := Trim(Copy(ALine, P + 1, MaxInt));
  end
  else
  begin
    AKey := Trim(ALine);
    ADesc := '';
  end;
end;

function KindName(AKind: TAiJevQuestionKind): string;
begin
  case AKind of
    jqChoice: Result := 'choice';
    jqScore: Result := 'score';
  else
    Result := 'noul';
  end;
end;

{ EAiJevError }

constructor EAiJevError.Create(const AMsg: string; AStatusCode: Integer);
begin
  inherited Create(AMsg);
  FStatusCode := AStatusCode;
end;

{ TAiJevQuestion }

constructor TAiJevQuestion.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FKind := jqChoice;
  FCriteria := TStringList.Create;
end;

destructor TAiJevQuestion.Destroy;
begin
  FCriteria.Free;
  inherited;
end;

procedure TAiJevQuestion.Assign(Source: TPersistent);
begin
  if Source is TAiJevQuestion then
  begin
    FName := TAiJevQuestion(Source).FName;
    FKind := TAiJevQuestion(Source).FKind;
    FInstructions := TAiJevQuestion(Source).FInstructions;
    FCriteria.Assign(TAiJevQuestion(Source).FCriteria);
  end
  else
    inherited;
end;

function TAiJevQuestion.GetDisplayName: string;
begin
  if FName <> '' then
    Result := FName + ' (' + KindName(FKind) + ')'
  else
    Result := inherited GetDisplayName;
end;

procedure TAiJevQuestion.SetCriteria(const Value: TStrings);
begin
  FCriteria.Assign(Value);
end;

function TAiJevQuestion.CriteriaLines: TArray<string>;
var
  i: Integer;
  L: TList<string>;
begin
  L := TList<string>.Create;
  try
    for i := 0 to FCriteria.Count - 1 do
      if Trim(FCriteria[i]) <> '' then
        L.Add(Trim(FCriteria[i]));
    Result := L.ToArray;
  finally
    L.Free;
  end;
end;

procedure TAiJevQuestion.Validate;
var
  Lines: TArray<string>;
  Line, Key, Desc: string;
  Seen: TDictionary<string, Boolean>;
begin
  if Trim(FName) = '' then
    raise EAiJevError.Create('Jev: una pregunta no tiene Name');
  if Trim(FInstructions) = '' then
    raise EAiJevError.CreateFmt('Jev: la pregunta "%s" no tiene Instructions', [FName]);

  Lines := CriteriaLines;
  case FKind of
    jqChoice:
      begin
        if (Length(Lines) < 2) or (Length(Lines) > JEV_MAX_CHOICE_OPTIONS) then
          raise EAiJevError.CreateFmt('Jev: la pregunta "%s" tiene %d opciones; un Choice admite de 2 a %d',
            [FName, Length(Lines), JEV_MAX_CHOICE_OPTIONS]);
        Seen := TDictionary<string, Boolean>.Create;
        try
          for Line in Lines do
          begin
            SplitOption(Line, Key, Desc);
            if Key = '' then
              raise EAiJevError.CreateFmt('Jev: la pregunta "%s" tiene una opcion sin clave: "%s"', [FName, Line]);
            if Seen.ContainsKey(Key) then
              raise EAiJevError.CreateFmt('Jev: la pregunta "%s" repite la opcion "%s"', [FName, Key]);
            Seen.Add(Key, True);
          end;
        finally
          Seen.Free;
        end;
      end;
    jqScore:
      if (Length(Lines) < JEV_MIN_SCORE_LEVELS) or (Length(Lines) > JEV_MAX_SCORE_LEVELS) then
        raise EAiJevError.CreateFmt('Jev: la pregunta "%s" tiene %d niveles; un Score admite de %d a %d',
          [FName, Length(Lines), JEV_MIN_SCORE_LEVELS, JEV_MAX_SCORE_LEVELS]);
    jqNoul:
      ; // criteria opcional
  end;
end;

function TAiJevQuestion.ToJSON: TJSONObject;
var
  Crit: TJSONObject;
  Arr: TJSONArray;
  Line, Key, Desc, WhenTrue, WhenFalse: string;
begin
  Result := TJSONObject.Create;
  try
    Result.AddPair('type', KindName(FKind));
    Result.AddPair('instructions', FInstructions);
    case FKind of
      jqChoice:
        begin
          Crit := TJSONObject.Create;
          Result.AddPair('criteria', Crit);
          for Line in CriteriaLines do
          begin
            SplitOption(Line, Key, Desc);
            if Desc = '' then
              Crit.AddPair(Key, TJSONNull.Create)
            else
              Crit.AddPair(Key, Desc);
          end;
        end;
      jqScore:
        begin
          Arr := TJSONArray.Create;
          Result.AddPair('criteria', Arr);
          for Line in CriteriaLines do
            Arr.Add(Line);
        end;
      jqNoul:
        begin
          WhenTrue := '';
          WhenFalse := '';
          for Line in CriteriaLines do
          begin
            SplitOption(Line, Key, Desc);
            if SameText(Key, 'true') then
              WhenTrue := Desc
            else if SameText(Key, 'false') then
              WhenFalse := Desc;
          end;
          if (WhenTrue <> '') or (WhenFalse <> '') then
          begin
            Crit := TJSONObject.Create;
            Result.AddPair('criteria', Crit);
            if WhenTrue <> '' then
              Crit.AddPair('true', WhenTrue);
            if WhenFalse <> '' then
              Crit.AddPair('false', WhenFalse);
          end;
        end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

{ TAiJevQuestions }

constructor TAiJevQuestions.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TAiJevQuestion);
end;

function TAiJevQuestions.GetItem(Index: Integer): TAiJevQuestion;
begin
  Result := TAiJevQuestion(inherited GetItem(Index));
end;

function TAiJevQuestions.Add: TAiJevQuestion;
begin
  Result := TAiJevQuestion(inherited Add);
end;

function TAiJevQuestions.AddChoice(const AName, AInstructions: string;
  const AOptions: array of string): TAiJevQuestion;
var
  S: string;
begin
  Result := Add;
  Result.Name := AName;
  Result.Kind := jqChoice;
  Result.Instructions := AInstructions;
  for S in AOptions do
    Result.Criteria.Add(S);
end;

function TAiJevQuestions.AddScore(const AName, AInstructions: string;
  const ALevels: array of string): TAiJevQuestion;
var
  S: string;
begin
  Result := Add;
  Result.Name := AName;
  Result.Kind := jqScore;
  Result.Instructions := AInstructions;
  for S in ALevels do
    Result.Criteria.Add(S);
end;

function TAiJevQuestions.AddNoul(const AName, AInstructions, AWhenTrue,
  AWhenFalse: string): TAiJevQuestion;
begin
  Result := Add;
  Result.Name := AName;
  Result.Kind := jqNoul;
  Result.Instructions := AInstructions;
  if AWhenTrue <> '' then
    Result.Criteria.Add('true=' + AWhenTrue);
  if AWhenFalse <> '' then
    Result.Criteria.Add('false=' + AWhenFalse);
end;

function TAiJevQuestions.Find(const AName: string): TAiJevQuestion;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if SameText(Items[i].Name, AName) then
      Exit(Items[i]);
  Result := nil;
end;

procedure TAiJevQuestions.Validate;
var
  i: Integer;
  Seen: TDictionary<string, Boolean>;
begin
  if Count = 0 then
    raise EAiJevError.Create('Jev: no hay preguntas');
  Seen := TDictionary<string, Boolean>.Create;
  try
    for i := 0 to Count - 1 do
    begin
      Items[i].Validate;
      if Seen.ContainsKey(Items[i].Name) then
        raise EAiJevError.CreateFmt('Jev: el nombre de pregunta "%s" esta repetido', [Items[i].Name]);
      Seen.Add(Items[i].Name, True);
    end;
  finally
    Seen.Free;
  end;
end;

{ TAiJevAnswer }

constructor TAiJevAnswer.Create;
begin
  inherited Create;
  FProbabilities := TDictionary<string, Double>.Create;
end;

destructor TAiJevAnswer.Destroy;
begin
  FProbabilities.Free;
  inherited;
end;

function TAiJevAnswer.Probability(const AOption: string): Double;
begin
  if not FProbabilities.TryGetValue(AOption, Result) then
    Result := 0;
end;

function TAiJevAnswer.Ranked: TArray<TPair<string, Double>>;
begin
  Result := FProbabilities.ToArray;
  TArray.Sort<TPair<string, Double>>(Result, TComparer<TPair<string, Double>>.Construct(
    function(const L, R: TPair<string, Double>): Integer
    begin
      Result := CompareValue(R.Value, L.Value);
      if Result = 0 then
        Result := CompareStr(L.Key, R.Key); // orden estable ante empates
    end));
end;

function TAiJevAnswer.Top(ACount: Integer): TArray<string>;
var
  R: TArray<TPair<string, Double>>;
  i: Integer;
begin
  R := Ranked;
  SetLength(Result, Min(Max(ACount, 0), Length(R)));
  for i := 0 to High(Result) do
    Result[i] := R[i].Key;
end;

{ TAiJevResult }

constructor TAiJevResult.Create;
begin
  inherited Create;
  FAnswers := TObjectDictionary<string, TAiJevAnswer>.Create([doOwnsValues]);
end;

destructor TAiJevResult.Destroy;
begin
  FAnswers.Free;
  inherited;
end;

function TAiJevResult.GetAnswer(const AName: string): TAiJevAnswer;
begin
  if not FAnswers.TryGetValue(AName, Result) then
    raise EAiJevError.CreateFmt('Jev: no hay respuesta para la pregunta "%s"', [AName]);
end;

function TAiJevResult.GetCount: Integer;
begin
  Result := FAnswers.Count;
end;

function TAiJevResult.TryGetAnswer(const AName: string; out AAnswer: TAiJevAnswer): Boolean;
begin
  Result := FAnswers.TryGetValue(AName, AAnswer);
end;

class function TAiJevResult.FromJSON(const AJson: string): TAiJevResult;
var
  Root: TJSONValue;
  Obj, Usage, Answers, A, Probs: TJSONObject;
  Pair, PP: TJSONPair;
  Ans: TAiJevAnswer;
  Kind: string;
begin
  Root := TJSONObject.ParseJSONValue(AJson);
  try
    if not (Root is TJSONObject) then
      raise EAiJevError.Create('Jev: la respuesta no es un objeto JSON');
    Obj := TJSONObject(Root);
    if not Obj.TryGetValue<TJSONObject>('answers', Answers) then
      raise EAiJevError.Create('Jev: la respuesta no trae "answers"');

    Result := TAiJevResult.Create;
    try
      Obj.TryGetValue<string>('model', Result.FModel);
      if Obj.TryGetValue<TJSONObject>('usage', Usage) then
      begin
        Usage.TryGetValue<Integer>('input_tokens', Result.FInputTokens);
        Usage.TryGetValue<Integer>('output_tokens', Result.FOutputTokens);
      end;

      for Pair in Answers do
      begin
        if not (Pair.JsonValue is TJSONObject) then
          Continue;
        A := TJSONObject(Pair.JsonValue);
        Ans := TAiJevAnswer.Create;
        Ans.FName := Pair.JsonString.Value;
        Result.FAnswers.AddOrSetValue(Ans.FName, Ans);

        Kind := '';
        A.TryGetValue<string>('type', Kind);
        if Kind = 'choice' then
        begin
          Ans.FKind := jqChoice;
          A.TryGetValue<string>('choice', Ans.FChoice);
        end
        else if Kind = 'score' then
        begin
          Ans.FKind := jqScore;
          A.TryGetValue<Double>('score', Ans.FScore);
        end
        else if Kind = 'noul' then
        begin
          Ans.FKind := jqNoul;
          A.TryGetValue<Double>('noul', Ans.FNoul);
        end
        else
          raise EAiJevError.CreateFmt('Jev: tipo de respuesta desconocido "%s" en "%s"', [Kind, Ans.FName]);

        A.TryGetValue<Double>('confidence', Ans.FConfidence);
        if A.TryGetValue<TJSONObject>('probabilities', Probs) then
          for PP in Probs do
            if PP.JsonValue is TJSONNumber then
              Ans.FProbabilities.AddOrSetValue(PP.JsonString.Value, TJSONNumber(PP.JsonValue).AsDouble);
      end;
    except
      Result.Free;
      raise;
    end;
  finally
    Root.Free;
  end;
end;

{ TAiJev }

constructor TAiJev.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FApiKey := '@TYPESAFE_API_KEY';
  FModel := 'jev-1.13.0';
  FUrl := JEV_DEFAULT_URL;
  FQuestions := TAiJevQuestions.Create(Self);
  FMaxRetries := 2;
  FRetryDelay := 500;
  FTimeout := 30000;
  FUsage := TAiJevUsageMeter.Create;
  FPricePerMillionInput := JEV_PRICE_PER_MILLION_INPUT;
end;

destructor TAiJev.Destroy;
begin
  FQuestions.Free;
  FUsage.Free;
  inherited;
end;

function TAiJev.GetUsage: TAiJevUsage;
begin
  Result := FUsage.Snapshot(JevEffectiveInputPrice(FUrl, FPricePerMillionInput), FPricePerMillionOutput);
end;

procedure TAiJev.ResetUsage;
begin
  FUsage.Reset;
end;

function TAiJev.GetApiKey: string;
begin
  if (csDesigning in ComponentState) then
    Exit(FApiKey);
  if FApiKey.StartsWith('@') then
    Result := GetEnvironmentVariable(Copy(FApiKey, 2, MaxInt))
  else
    Result := FApiKey;
end;

procedure TAiJev.SetQuestions(const Value: TAiJevQuestions);
begin
  FQuestions.Assign(Value);
end;

function TAiJev.BuildRequest(AState: TJSONValue; AQuestions: TAiJevQuestions): TJSONObject;
begin
  Result := BuildRequest(AState, AQuestions, []);
end;

function TAiJev.BuildRequest(AState: TJSONValue; AQuestions: TAiJevQuestions;
  const AImages: array of TAiMediaFile): TJSONObject;
var
  Qs: TJSONObject;
  Imgs: TJSONArray;
  Encoded: TArray<string>;
  Data: TBytes;
  B64: TBase64Encoding;
  i: Integer;
begin
  if AState = nil then
    raise EAiJevError.Create('Jev: el state no puede ser nil');
  AQuestions.Validate;

  // Imagenes: base64 crudo (sin data URL ni saltos de linea), solo PNG, JPEG
  // o WebP; se valida por los bytes, no por la extension del archivo
  SetLength(Encoded, Length(AImages));
  if Length(AImages) > 0 then
  begin
    B64 := TBase64Encoding.Create(0);
    try
      for i := 0 to High(AImages) do
      begin
        if (AImages[i] = nil) or (AImages[i].Content = nil) or (AImages[i].Content.Size = 0) then
          raise EAiJevError.CreateFmt('Jev: la imagen %d esta vacia', [i + 1]);
        SetLength(Data, AImages[i].Content.Size);
        AImages[i].Content.Position := 0;
        AImages[i].Content.ReadBuffer(Data[0], Length(Data));
        if JevImageKind(Data) = '' then
          raise EAiJevError.CreateFmt('Jev: la imagen %d ("%s") no es PNG, JPEG ni WebP',
            [i + 1, AImages[i].filename]);
        Encoded[i] := B64.EncodeBytesToString(Data);
      end;
    finally
      B64.Free;
    end;
  end;

  Result := TJSONObject.Create;
  try
    Result.AddPair('model', FModel);
    Result.AddPair('state', TJSONValue(AState.Clone));
    if Length(Encoded) > 0 then
    begin
      Imgs := TJSONArray.Create;
      Result.AddPair('images', Imgs);
      for i := 0 to High(Encoded) do
        Imgs.Add(Encoded[i]);
    end;
    Qs := TJSONObject.Create;
    Result.AddPair('questions', Qs);
    for i := 0 to AQuestions.Count - 1 do
      Qs.AddPair(AQuestions[i].Name, AQuestions[i].ToJSON);
  except
    Result.Free;
    raise;
  end;
end;

function TAiJev.DoPost(const ABody: string; out AResponse: string): Integer;
var
  HTTP: TNetHTTPClient;
  Body: TStringStream;
  Resp: IHTTPResponse;
begin
  HTTP := TNetHTTPClient.Create(nil);
  try
{$IF CompilerVersion >= 34}
    HTTP.SynchronizeEvents := False;
{$IFEND}
    HTTP.ConnectionTimeout := FTimeout;
    HTTP.ResponseTimeout := FTimeout;
    HTTP.ContentType := 'application/json';
    // UTF-8 explicito: con la codificacion por defecto la API rechaza tildes y signos (400)
    Body := TStringStream.Create(ABody, TEncoding.UTF8);
    try
      Resp := HTTP.Post(FUrl + 'systemone', Body, nil,
        [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)]);
    finally
      Body.Free;
    end;
    AResponse := Resp.ContentAsString(TEncoding.UTF8);
    Result := Resp.StatusCode;
  finally
    HTTP.Free;
  end;
end;

function TAiJev.Ask(const AState: string; AQuestions: TAiJevQuestions): TAiJevResult;
var
  S: TJSONString;
begin
  S := TJSONString.Create(AState);
  try
    Result := Ask(S, AQuestions);
  finally
    S.Free;
  end;
end;

function TAiJev.Ask(const AState: string; const AImages: array of TAiMediaFile;
  AQuestions: TAiJevQuestions): TAiJevResult;
var
  S: TJSONString;
begin
  S := TJSONString.Create(AState);
  try
    Result := Ask(S, AImages, AQuestions);
  finally
    S.Free;
  end;
end;

function TAiJev.Ask(AState: TJSONValue; AQuestions: TAiJevQuestions): TAiJevResult;
begin
  Result := Ask(AState, [], AQuestions);
end;

function TAiJev.Ask(AState: TJSONValue; const AImages: array of TAiMediaFile;
  AQuestions: TAiJevQuestions): TAiJevResult;
var
  Req: TJSONObject;
  Body, Resp: string;
  Status, Attempt: Integer;
  Span: TAiSpan;
begin
  if AQuestions = nil then
    AQuestions := FQuestions;
  FLastError := '';
  Result := nil;

  Span := AiSpanStart('jev.ask', skClient);
  try
    if JevIsTypeSafeUrl(FUrl) then
      AiSpanAttr(Span, 'gen_ai.system', 'typesafe')
    else
      AiSpanAttr(Span, 'gen_ai.system', 'systemone'); // Ollama u otro servidor
    AiSpanAttr(Span, 'gen_ai.request.model', FModel);
    AiSpanAttr(Span, 'jev.questions', Int64(AQuestions.Count));
    AiSpanAttr(Span, 'jev.images', Int64(Length(AImages)));

    Req := BuildRequest(AState, AQuestions, AImages);
    try
      Body := Req.ToJSON;
    finally
      Req.Free;
    end;

    Attempt := 0;
    repeat
      Status := DoPost(Body, Resp);
      // 429 rate limit, 529 sobrecarga (docs), 503 no disponible: transitorios
      if not ((Status = 429) or (Status = 503) or (Status = 529)) or (Attempt >= FMaxRetries) then
        Break;
      Inc(Attempt);
      if FRetryDelay > 0 then
        Sleep(FRetryDelay * (1 shl (Attempt - 1)));
    until False;
    AiSpanAttr(Span, 'http.response.status_code', Int64(Status));
    AiSpanAttr(Span, 'jev.retries', Int64(Attempt));

    if Status <> 200 then
      raise EAiJevError.Create(Format('Jev: HTTP %d: %s', [Status, Resp]), Status);

    Result := TAiJevResult.FromJSON(Resp);
    AiSpanAttr(Span, 'gen_ai.response.model', Result.Model);
    AiSpanAttr(Span, 'gen_ai.usage.input_tokens', Int64(Result.InputTokens));
    AiSpanAttr(Span, 'gen_ai.usage.output_tokens', Int64(Result.OutputTokens));
    AiSpanEnd(Span);
    FUsage.Add(Result);
    if Assigned(FOnUsage) then
    begin
      var U: TAiJevUsage;
      U.Requests := 1;
      U.InputTokens := Result.InputTokens;
      U.OutputTokens := Result.OutputTokens;
      U.CostUSD := (U.InputTokens * JevEffectiveInputPrice(FUrl, FPricePerMillionInput) +
        U.OutputTokens * FPricePerMillionOutput) / 1E6;
      FOnUsage(Self, U);
    end;
  except
    on E: Exception do
    begin
      FreeAndNil(Result);
      FLastError := E.Message;
      AiSpanEnd(Span, E.Message);
      if Assigned(FOnError) then
        FOnError(Self, E.Message, E, nil);
      raise;
    end;
  end;
end;

function TAiJev.Choose(const AState, AInstructions: string; const AOptions: array of string;
  out AConfidence: Double): string;
var
  Q: TAiJevQuestions;
  R: TAiJevResult;
begin
  Q := TAiJevQuestions.Create(nil);
  try
    Q.AddChoice('q', AInstructions, AOptions);
    R := Ask(AState, Q);
    try
      Result := R['q'].Choice;
      AConfidence := R['q'].Confidence;
    finally
      R.Free;
    end;
  finally
    Q.Free;
  end;
end;

function TAiJev.Noul(const AState, AInstructions: string): Double;
var
  Q: TAiJevQuestions;
  R: TAiJevResult;
begin
  Q := TAiJevQuestions.Create(nil);
  try
    Q.AddNoul('q', AInstructions);
    R := Ask(AState, Q);
    try
      Result := R['q'].Noul;
    finally
      R.Free;
    end;
  finally
    Q.Free;
  end;
end;

end.
