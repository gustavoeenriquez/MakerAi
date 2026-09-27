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
// Jev el riesgo de cada tool call que las listas (AllowedTools, BlockedTools,
// BlockedArgPatterns) dejaron pasar. Complementa las listas: estas atrapan lo
// que se puede enumerar ('rm -rf', 'DROP TABLE'); Jev atrapa lo que no, como un
// correo a un externo con una clave en el cuerpo o una transferencia a una
// cuenta desconocida.
//
//   Guardrails.Classifier := JevGuard;   // este componente
//
// Una pregunta Noul sobre {tool, arguments}; si P(riesgo) >= BlockThreshold,
// bloquea. Policy es la pregunta y define que cuenta como riesgo: ajustarla al
// dominio (p.ej. agregar "modificar registros contables ya cerrados").
//
// Si Jev no responde (red, 401, ...): BlockOnError = True (defecto) bloquea;
// False deja pasar. El motivo del bloqueo queda en AReason y en OnBlocked.
//
// Calibrado contra jev-1.13.0 (sep 27/2026) sobre 13 tool calls: los seguros
// dieron <= 0.17 y los peligrosos >= 0.88 (rm -rf, DELETE sin WHERE, hosts,
// correo con clave a externo, transferencia a cuenta desconocida, curl | sh,
// escalada de rol por UPDATE).
// -----------------------------------------------------------------------------

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  uMakerAi.Guardrails, uMakerAi.Jev;

type
  TAiJevGuardrailClassifier = class(TAiGuardrailClassifierBase)
  private
    FJev: TAiJev;
    FOwnJev: TAiJev;
    FApiKey: string;
    FModel: string;
    FPolicy: string;
    FBlockThreshold: Double;
    FBlockOnError: Boolean;
    FLastRisk: Double;
    procedure SetJev(const Value: TAiJev);
    function ActiveJev: TAiJev;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public const
    DEFAULT_POLICY =
      'Could executing the tool `tool` with the arguments `arguments` cause harm: deleting or ' +
      'overwriting data, exposing credentials or private information, moving money, or running ' +
      'destructive system commands?';
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CheckToolCall(const AToolName, AArguments: string; out AReason: string): Boolean; override;
    // Probabilidad de riesgo del ultimo tool call evaluado (-1 si Jev fallo)
    property LastRisk: Double read FLastRisk;
  published
    // Opcional: TAiJev externo (compartido o doble de pruebas); si se asigna,
    // ApiKey y Model de este componente se ignoran
    property Jev: TAiJev read FJev write SetJev;
    property ApiKey: string read FApiKey write FApiKey;
    property Model: string read FModel write FModel;
    // Pregunta si/no sobre `tool` y `arguments`; vacio = DEFAULT_POLICY
    property Policy: string read FPolicy write FPolicy;
    property BlockThreshold: Double read FBlockThreshold write FBlockThreshold;
    property BlockOnError: Boolean read FBlockOnError write FBlockOnError default True;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiJevGuardrailClassifier]);
end;

{ TAiJevGuardrailClassifier }

constructor TAiJevGuardrailClassifier.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FApiKey := '@TYPESAFE_API_KEY';
  FModel := 'jev-1.13.0';
  FPolicy := '';
  FBlockThreshold := 0.5;
  FBlockOnError := True;
  FLastRisk := -1;
end;

destructor TAiJevGuardrailClassifier.Destroy;
begin
  FOwnJev.Free;
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
  Result := FOwnJev;
end;

function TAiJevGuardrailClassifier.CheckToolCall(const AToolName, AArguments: string;
  out AReason: string): Boolean;
var
  Q: TAiJevQuestions;
  State: TJSONObject;
  R: TAiJevResult;
  Instr: string;
begin
  Result := True;
  AReason := '';
  FLastRisk := -1;

  Instr := FPolicy;
  if Trim(Instr) = '' then
    Instr := DEFAULT_POLICY;

  try
    Q := TAiJevQuestions.Create(nil);
    try
      Q.AddNoul('risk', Instr);
      State := TJSONObject.Create;
      try
        State.AddPair('tool', AToolName);
        // Los argumentos van como texto JSON crudo, igual que los recibe el guardrail
        State.AddPair('arguments', AArguments);
        R := ActiveJev.Ask(State, Q);
      finally
        State.Free;
      end;
      try
        FLastRisk := R['risk'].Noul;
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
    AReason := Format('Jev risk %s >= %s', [
      FormatFloat('0.00', FLastRisk, TFormatSettings.Invariant),
      FormatFloat('0.00', FBlockThreshold, TFormatSettings.Invariant)]);
  end;
end;

end.
