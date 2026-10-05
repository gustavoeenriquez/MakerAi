// MIT License
//
// MakerAI - Jev: clasificador de riesgo de tool calls para TAiGuardrails
//
// Nombre: Gustavo Enriquez
// - Email: gustavoeenriquez@gmail.com
// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.Jev.Guardrails;

// -----------------------------------------------------------------------------
// TAiJevGuardrailClassifier: se asigna a TAiGuardrails.Classifier y juzga con
// Jev cada tool call que las listas (AllowedTools, BlockedTools,
// BlockedArgPatterns) dejaron pasar. Complementa las listas: estas atrapan lo
// que se puede enumerar ('rm -rf', 'DROP TABLE'); Jev atrapa lo que no, como un
// correo a un externo con una clave en el cuerpo o una transferencia a una
// cuenta desconocida.
//
//   Guardrails.Classifier := JevGuard;   // este componente
//
// Dos juicios en UNA llamada sobre {tool, description?, arguments}:
//
//   1. Riesgo (Noul, siempre): si P(riesgo) >= BlockThreshold, bloquea. Policy
//      es la pregunta y define que cuenta como riesgo: ajustarla al dominio.
//      Vacia: sin Categories se usa DEFAULT_POLICY (estricta: cualquier pago
//      o borrado cuenta como riesgo); con Categories, ABUSE_POLICY (solo lo
//      abusivo), porque el control por tipo de operacion ya lo hacen las
//      categorias. Con DEFAULT_POLICY un pay_invoice normal daba 0.88.
//   2. Categoria de permiso (Choice, opcional): si Categories no esta vacio,
//      clasifica la llamada ('read', 'write', 'financial', ...) y bloquea si la
//      probabilidad de alguna de BlockedCategories alcanza CategoryThreshold
//      (aunque no sea la mas probable: ante la duda entre 'write' y
//      'financial', manda la politica de 'financial'). OnCategorized permite
//      auditar cada llamada y cambiar la decision.
//
// ToolDescriptions ('nombre=descripcion') agrega al state una descripcion de
// la tool. Hace falta con nombres de dominio que Jev lee al pie de la letra:
// 'anular_comprobante' salia 'read' (0.83); con "genera el comprobante de
// reversion" sale 'write' (0.95).
//
// Si Jev no responde (red, 401, ...): BlockOnError = True (defecto) bloquea;
// False deja pasar. El motivo del bloqueo queda en AReason y en OnBlocked.
//
// Calibrado contra jev-1.13.0 (sep 27/2026). Riesgo, 13 tool calls: seguros
// <= 0.17, peligrosos >= 0.88. Categorias, 20 tool calls en 6 categorias:
// 19/20 sin descripciones, 20/20 con la descripcion de anular_comprobante.
// -----------------------------------------------------------------------------

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  uMakerAi.Guardrails, uMakerAi.Jev;

type
  // Auditoria de la categoria; AAllow/AReason llegan con la decision tomada
  // (riesgo y categorias bloqueadas) y el handler puede cambiarla.
  TAiJevToolCategorizedEvent = procedure(Sender: TObject; const AToolName, AArguments,
    ACategory: string; AConfidence: Double; var AAllow: Boolean; var AReason: string) of object;

  TAiJevGuardrailClassifier = class(TAiGuardrailClassifierBase)
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
    FPolicy: string;
    FBlockThreshold: Double;
    FBlockOnError: Boolean;
    FLastRisk: Double;
    FCategories: TStrings;
    FBlockedCategories: TStrings;
    FCategoryThreshold: Double;
    FToolDescriptions: TStrings;
    FLastCategory: string;
    FLastCategoryConfidence: Double;
    FOnCategorized: TAiJevToolCategorizedEvent;
    procedure SetJev(const Value: TAiJev);
    procedure SetCategories(const Value: TStrings);
    procedure SetBlockedCategories(const Value: TStrings);
    procedure SetToolDescriptions(const Value: TStrings);
    function ActiveJev: TAiJev;
    function GetUsage: TAiJevUsage;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public const
    DEFAULT_POLICY =
      'Could executing the tool `tool` with the arguments `arguments` cause harm: deleting or ' +
      'overwriting data, exposing credentials or private information, moving money, or running ' +
      'destructive system commands?';
    // Calibrada sobre 19 tool calls (13 de riesgo + 6 operaciones legitimas de
    // negocio): peligrosos >= 0.64, legitimos <= 0.35, 0 errores
    ABUSE_POLICY =
      'Does calling the tool `tool` with the arguments `arguments` look abusive or malicious rather ' +
      'than a normal business operation: destroying data in bulk, leaking credentials or private ' +
      'data, sending money to unknown parties, or escalating privileges?';
    CATEGORY_QUESTION = 'Which kind of action does calling `tool` with `arguments` perform?';
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Consumo de Jev acumulado desde Create o ResetUsage (seguro entre hilos)
    property Usage: TAiJevUsage read GetUsage;
    procedure ResetUsage;
    function CheckToolCall(const AToolName, AArguments: string; out AReason: string): Boolean; override;
    // Probabilidad de riesgo del ultimo tool call evaluado (-1 si Jev fallo)
    property LastRisk: Double read FLastRisk;
    // Categoria y confianza del ultimo tool call ('' si no hay Categories o Jev fallo)
    property LastCategory: string read FLastCategory;
    property LastCategoryConfidence: Double read FLastCategoryConfidence;
  published
    // Opcional: TAiJev externo (compartido o doble de pruebas); si se asigna,
    // ApiKey y Model de este componente se ignoran
    property Jev: TAiJev read FJev write SetJev;
    property ApiKey: string read FApiKey write FApiKey;
    property Model: string read FModel write FModel;
    // Servidor System One: '' = TypeSafe; 'http://localhost:11434/v1/' = Ollama
    // (con Model := 'nimble', 'clef-flash'...). Ignorada si Jev esta asignado
    property Url: string read FUrl write FUrl;
    // Pregunta si/no sobre `tool` y `arguments`; vacio = DEFAULT_POLICY, o
    // ABUSE_POLICY si hay Categories
    property Policy: string read FPolicy write FPolicy;
    property BlockThreshold: Double read FBlockThreshold write FBlockThreshold;
    property BlockOnError: Boolean read FBlockOnError write FBlockOnError default True;
    // Categorias de permiso, una por linea 'clave=descripcion'. Vacio = no se categoriza
    property Categories: TStrings read FCategories write SetCategories;
    // Claves de Categories que se bloquean siempre
    property BlockedCategories: TStrings read FBlockedCategories write SetBlockedCategories;
    property CategoryThreshold: Double read FCategoryThreshold write FCategoryThreshold;
    // Opcional: 'nombre_tool=descripcion' para tools cuyo nombre no basta
    property ToolDescriptions: TStrings read FToolDescriptions write SetToolDescriptions;
    property OnCategorized: TAiJevToolCategorizedEvent read FOnCategorized write FOnCategorized;
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
  RegisterComponents('MakerAI', [TAiJevGuardrailClassifier]);
end;

function Inv(AValue: Double): string;
begin
  Result := FormatFloat('0.00', AValue, TFormatSettings.Invariant);
end;

{ TAiJevGuardrailClassifier }

constructor TAiJevGuardrailClassifier.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUsage := TAiJevUsageMeter.Create;
  FPricePerMillionInput := JEV_PRICE_PER_MILLION_INPUT;
  FApiKey := '@TYPESAFE_API_KEY';
  FModel := 'jev-1.13.0';
  FPolicy := '';
  FBlockThreshold := 0.5;
  FBlockOnError := True;
  FLastRisk := -1;
  FCategories := TStringList.Create;
  FBlockedCategories := TStringList.Create;
  FCategoryThreshold := 0.5;
  FToolDescriptions := TStringList.Create;
end;

destructor TAiJevGuardrailClassifier.Destroy;
begin
  FCategories.Free;
  FBlockedCategories.Free;
  FToolDescriptions.Free;
  FOwnJev.Free;
  FUsage.Free;
  inherited;
end;

procedure TAiJevGuardrailClassifier.SetJev(const Value: TAiJev);
begin
  if FJev = Value then
    Exit;
  if Assigned(FJev) then
    FJev.RemoveFreeNotification(Self);
  FJev := Value;
  if Assigned(FJev) then
    FJev.FreeNotification(Self);
end;

procedure TAiJevGuardrailClassifier.SetCategories(const Value: TStrings);
begin
  FCategories.Assign(Value);
end;

procedure TAiJevGuardrailClassifier.SetBlockedCategories(const Value: TStrings);
begin
  FBlockedCategories.Assign(Value);
end;

procedure TAiJevGuardrailClassifier.SetToolDescriptions(const Value: TStrings);
begin
  FToolDescriptions.Assign(Value);
end;

procedure TAiJevGuardrailClassifier.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FJev) then
    FJev := nil;
end;

function TAiJevGuardrailClassifier.ActiveJev: TAiJev;
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

function TAiJevGuardrailClassifier.CheckToolCall(const AToolName, AArguments: string;
  out AReason: string): Boolean;
var
  Q: TAiJevQuestions;
  QC: TAiJevQuestion;
  State: TJSONObject;
  R: TAiJevResult;
  Instr, Desc, Key: string;
  UseCategories: Boolean;
  i: Integer;
  P, WorstP: Double;
  WorstKey: string;
begin
  Result := True;
  AReason := '';
  FLastRisk := -1;
  FLastCategory := '';
  FLastCategoryConfidence := 0;
  WorstP := -1;
  WorstKey := '';

  UseCategories := FCategories.Count > 0;
  Instr := FPolicy;
  if Trim(Instr) = '' then
    if UseCategories then
      Instr := ABUSE_POLICY
    else
      Instr := DEFAULT_POLICY;

  try
    Q := TAiJevQuestions.Create(nil);
    try
      Q.AddNoul('risk', Instr);
      if UseCategories then
      begin
        QC := Q.Add;
        QC.Name := 'category';
        QC.Kind := jqChoice;
        QC.Instructions := CATEGORY_QUESTION;
        QC.Criteria.Assign(FCategories);
      end;
      State := TJSONObject.Create;
      try
        State.AddPair('tool', AToolName);
        Desc := Trim(FToolDescriptions.Values[AToolName]);
        if Desc <> '' then
          State.AddPair('description', Desc);
        // Los argumentos van como texto JSON crudo, igual que los recibe el guardrail
        State.AddPair('arguments', AArguments);
        R := ActiveJev.Ask(State, Q);
      finally
        State.Free;
      end;
      try
        JevReportResult(Self, R, FUsage, JevAdapterInputPrice(FJev, FUrl, FPricePerMillionInput), FPricePerMillionOutput, FOnUsage);
        FLastRisk := R['risk'].Noul;
        if UseCategories then
        begin
          FLastCategory := R['category'].Choice;
          FLastCategoryConfidence := R['category'].Confidence;
          // La categoria bloqueada mas probable, aunque no sea la eleccion
          for i := 0 to FBlockedCategories.Count - 1 do
          begin
            Key := Trim(FBlockedCategories[i]);
            if Key = '' then
              Continue;
            P := R['category'].Probability(Key);
            if P > WorstP then
            begin
              WorstP := P;
              WorstKey := Key;
            end;
          end;
        end;
      finally
        R.Free;
      end;
    finally
      Q.Free;
    end;
  except
    on E: Exception do
    begin
      if FBlockOnError then
      begin
        AReason := 'Jev guardrail unavailable: ' + E.Message;
        Exit(False);
      end;
      Exit(True);
    end;
  end;

  if FLastRisk >= FBlockThreshold then
  begin
    Result := False;
    AReason := Format('Jev risk %s >= %s', [Inv(FLastRisk), Inv(FBlockThreshold)]);
  end
  else if (WorstKey <> '') and (WorstP >= FCategoryThreshold) then
  begin
    Result := False;
    AReason := Format('Jev category %s %s >= %s is blocked', [WorstKey, Inv(WorstP), Inv(FCategoryThreshold)]);
  end;

  if UseCategories and Assigned(FOnCategorized) then
    FOnCategorized(Self, AToolName, AArguments, FLastCategory, FLastCategoryConfidence, Result, AReason);
end;

function TAiJevGuardrailClassifier.GetUsage: TAiJevUsage;
begin
  Result := FUsage.Snapshot(JevAdapterInputPrice(FJev, FUrl, FPricePerMillionInput), FPricePerMillionOutput);
end;

procedure TAiJevGuardrailClassifier.ResetUsage;
begin
  FUsage.Reset;
end;

end.
