// MIT License
//
// MakerAI - Jev: juez calibrado para TAiEvalRunner con el modelo Jev (TypeSafe AI)
//
// Nombre: Gustavo Enriquez
// - Email: gustavoeenriquez@gmail.com
// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.Jev.Evals;

// -----------------------------------------------------------------------------
// TAiJevEvalScorer: se asigna a TAiEvalRunner.Scorer y responde los checks
// ExpectScore('criterio', minimo) con la probabilidad calibrada de que la
// salida cumpla el criterio. Frente a ExpectJudge (un LLM que dice PASS/FAIL)
// es mas rapido y barato, y el minimo se ajusta en codigo.
//
//   Runner.Scorer := JevScorer;          // este componente
//   Runner.AddCase('cortesia')
//     .Input('Mi factura esta mal')
//     .ExpectScore('Es cortes y profesional', 0.7);
//
// Una pregunta Noul sobre {input, response}: "Does `response` satisfy this
// criterion: <criterio>?". Un error de Jev se propaga y el runner marca el
// check como fallido con el motivo.
//
// Calibrado contra jev-1.13.0 (sep 27/2026) sobre 10 pares criterio/respuesta
// en espanol (idioma, dato contable presente, cifras inventadas, negarse a
// revelar una clave, cortesia): los que cumplen >= 0.97, los que no <= 0.02.
// -----------------------------------------------------------------------------

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  uMakerAi.Evals, uMakerAi.Jev;

type
  TAiJevEvalScorer = class(TAiEvalScorerBase)
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
    FLastScore: Double;
    procedure SetJev(const Value: TAiJev);
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
    function Score(const ACriteria, AInput, AActual: string): Double; override;
    // Probabilidad del ultimo check evaluado (-1 si aun no se evaluo ninguno)
    property LastScore: Double read FLastScore;
  published
    // Opcional: TAiJev externo (compartido o doble de pruebas); si se asigna,
    // ApiKey y Model de este componente se ignoran
    property Jev: TAiJev read FJev write SetJev;
    property ApiKey: string read FApiKey write FApiKey;
    property Model: string read FModel write FModel;
    // Servidor System One: '' = TypeSafe; 'http://localhost:11434/v1/' = Ollama
    // (con Model := 'nimble', 'clef-flash'...). Ignorada si Jev esta asignado
    property Url: string read FUrl write FUrl;
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
  RegisterComponents('MakerAI', [TAiJevEvalScorer]);
end;

{ TAiJevEvalScorer }

constructor TAiJevEvalScorer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUsage := TAiJevUsageMeter.Create;
  FPricePerMillionInput := JEV_PRICE_PER_MILLION_INPUT;
  FApiKey := '@TYPESAFE_API_KEY';
  FModel := 'jev-1.13.0';
  FLastScore := -1;
end;

destructor TAiJevEvalScorer.Destroy;
begin
  FOwnJev.Free;
  FUsage.Free;
  inherited;
end;

procedure TAiJevEvalScorer.SetJev(const Value: TAiJev);
begin
  if FJev = Value then
    Exit;
  if Assigned(FJev) then
    FJev.RemoveFreeNotification(Self);
  FJev := Value;
  if Assigned(FJev) then
    FJev.FreeNotification(Self);
end;

procedure TAiJevEvalScorer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FJev) then
    FJev := nil;
end;

function TAiJevEvalScorer.ActiveJev: TAiJev;
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

function TAiJevEvalScorer.Score(const ACriteria, AInput, AActual: string): Double;
var
  Q: TAiJevQuestions;
  State: TJSONObject;
  R: TAiJevResult;
begin
  Q := TAiJevQuestions.Create(nil);
  try
    // Redaccion calibrada (10/10): el criterio va en la pregunta, no en el state
    Q.AddNoul('pass', 'Does `response` satisfy this criterion: ' + ACriteria + '?');
    State := TJSONObject.Create;
    try
      if AInput <> '' then
        State.AddPair('input', AInput);
      State.AddPair('response', AActual);
      R := ActiveJev.Ask(State, Q);
    finally
      State.Free;
    end;
    try
      JevReportResult(Self, R, FUsage, JevAdapterInputPrice(FJev, FUrl, FPricePerMillionInput), FPricePerMillionOutput, FOnUsage);
      Result := R['pass'].Noul;
    finally
      R.Free;
    end;
  finally
    Q.Free;
  end;
  FLastScore := Result;
end;

function TAiJevEvalScorer.GetUsage: TAiJevUsage;
begin
  Result := FUsage.Snapshot(JevAdapterInputPrice(FJev, FUrl, FPricePerMillionInput), FPricePerMillionOutput);
end;

procedure TAiJevEvalScorer.ResetUsage;
begin
  FUsage.Reset;
end;

end.
