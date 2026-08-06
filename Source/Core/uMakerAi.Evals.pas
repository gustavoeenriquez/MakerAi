// MIT License
//
// MakerAI - Evals: evaluacion sistematica de respuestas de IA (fase 3)
//
// Nombre: Gustavo Enriquez
// - Email: gustavoeenriquez@gmail.com
// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.Evals;

// -----------------------------------------------------------------------------
// TAiEvalRunner: mini-framework de evals para pipelines de IA en Delphi.
//
// El target es una funcion generica (input -> output), asi que sirve para
// evaluar un TAiChat, un grafo de agentes, un tool MCP, un agente A2A o
// cualquier funcion propia. Los casos se definen con API fluida:
//
//   Runner.AddCase('saludo')
//     .Input('Di hola en espanol')
//     .ExpectContains('hola')
//     .ExpectMaxLength(200);
//
//   Report := Runner.Run(
//     function(const AInput: string): string
//     begin
//       Result := Chat.AddMessageAndRun(AInput, 'user', []);
//     end);
//
// Checks deterministas: Contains / NotContains / Regex / Equals / MinLength /
// MaxLength. Check semantico: ExpectJudge('criterio') usa un TAiChat como
// LLM-as-judge (propiedad Judge) que responde PASS/FAIL contra el criterio.
//
// Cada caso emite un span OTel 'eval.case <name>' (via uMakerAi.Telemetry) con
// el resultado, de modo que los evals quedan trazados junto al resto del
// pipeline. El reporte ofrece ToText (consola) y ToJSON (CI / persistencia).
// -----------------------------------------------------------------------------

interface

uses
  System.SysUtils, System.StrUtils, System.Classes, System.JSON,
  System.Generics.Collections, System.Diagnostics, System.RegularExpressions,
  uMakerAi.Chat;

type
  TAiEvalCheckKind = (ekContains, ekNotContains, ekRegex, ekEquals,
    ekMinLength, ekMaxLength, ekJudge);

  TAiEvalCheck = class
  public
    Kind: TAiEvalCheckKind;
    Value: string; // texto esperado / patron / criterio del judge / longitud
  end;

  TAiEvalCase = class
  private
    FName: string;
    FInput: string;
    FChecks: TObjectList<TAiEvalCheck>;
    function AddCheck(AKind: TAiEvalCheckKind; const AValue: string): TAiEvalCase;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;
    function Input(const AInput: string): TAiEvalCase;
    function ExpectContains(const AText: string): TAiEvalCase;
    function ExpectNotContains(const AText: string): TAiEvalCase;
    function ExpectRegex(const APattern: string): TAiEvalCase;
    function ExpectEquals(const AText: string): TAiEvalCase;
    function ExpectMinLength(ALength: Integer): TAiEvalCase;
    function ExpectMaxLength(ALength: Integer): TAiEvalCase;
    // Criterio semantico evaluado por el LLM judge (propiedad Judge del runner)
    function ExpectJudge(const ACriteria: string): TAiEvalCase;
    property CaseName: string read FName;
  end;

  TAiEvalCaseResult = class
  public
    CaseName: string;
    Passed: Boolean;
    Actual: string;      // salida real del target (truncada a 4KB)
    FailReason: string;  // vacio si paso
    DurationMs: Int64;
  end;

  TAiEvalReport = class
  private
    FResults: TObjectList<TAiEvalCaseResult>;
    function GetPassed: Integer;
    function GetFailed: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function ToText: string;   // resumen legible para consola
    function ToJSON: TJSONObject; // el llamador libera el objeto
    property Results: TObjectList<TAiEvalCaseResult> read FResults;
    property Passed: Integer read GetPassed;
    property Failed: Integer read GetFailed;
    function AllPassed: Boolean;
  end;

  // Target generico: recibe el input del caso y devuelve la salida a evaluar
  TAiEvalTargetFunc = reference to function(const AInput: string): string;

  TAiEvalRunner = class(TComponent)
  private
    FCases: TObjectList<TAiEvalCase>;
    FJudge: TAiChat;
    procedure SetJudge(const Value: TAiChat);
    function RunJudge(const ACriteria, AActual: string; out AReason: string): Boolean;
    function EvaluateCase(ACase: TAiEvalCase; const AActual: string; out AReason: string): Boolean;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Define un caso nuevo (API fluida sobre el TAiEvalCase devuelto)
    function AddCase(const AName: string): TAiEvalCase;
    procedure Clear;

    // Ejecuta todos los casos contra el target. El llamador libera el reporte.
    function Run(ATarget: TAiEvalTargetFunc): TAiEvalReport;

    property Cases: TObjectList<TAiEvalCase> read FCases;
  published
    // Chat usado como LLM-as-judge para los checks ExpectJudge (opcional)
    property Judge: TAiChat read FJudge write SetJudge;
  end;

procedure Register;

implementation

uses uMakerAi.Telemetry;

const
  MAX_ACTUAL_STORED = 4096;

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiEvalRunner]);
end;

{ TAiEvalCase }

constructor TAiEvalCase.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  FChecks := TObjectList<TAiEvalCheck>.Create(True);
end;

destructor TAiEvalCase.Destroy;
begin
  FChecks.Free;
  inherited;
end;

function TAiEvalCase.AddCheck(AKind: TAiEvalCheckKind; const AValue: string): TAiEvalCase;
var
  C: TAiEvalCheck;
begin
  C := TAiEvalCheck.Create;
  C.Kind := AKind;
  C.Value := AValue;
  FChecks.Add(C);
  Result := Self;
end;

function TAiEvalCase.Input(const AInput: string): TAiEvalCase;
begin
  FInput := AInput;
  Result := Self;
end;

function TAiEvalCase.ExpectContains(const AText: string): TAiEvalCase;
begin
  Result := AddCheck(ekContains, AText);
end;

function TAiEvalCase.ExpectNotContains(const AText: string): TAiEvalCase;
begin
  Result := AddCheck(ekNotContains, AText);
end;

function TAiEvalCase.ExpectRegex(const APattern: string): TAiEvalCase;
begin
  Result := AddCheck(ekRegex, APattern);
end;

function TAiEvalCase.ExpectEquals(const AText: string): TAiEvalCase;
begin
  Result := AddCheck(ekEquals, AText);
end;

function TAiEvalCase.ExpectMinLength(ALength: Integer): TAiEvalCase;
begin
  Result := AddCheck(ekMinLength, IntToStr(ALength));
end;

function TAiEvalCase.ExpectMaxLength(ALength: Integer): TAiEvalCase;
begin
  Result := AddCheck(ekMaxLength, IntToStr(ALength));
end;

function TAiEvalCase.ExpectJudge(const ACriteria: string): TAiEvalCase;
begin
  Result := AddCheck(ekJudge, ACriteria);
end;

{ TAiEvalReport }

constructor TAiEvalReport.Create;
begin
  inherited;
  FResults := TObjectList<TAiEvalCaseResult>.Create(True);
end;

destructor TAiEvalReport.Destroy;
begin
  FResults.Free;
  inherited;
end;

function TAiEvalReport.GetPassed: Integer;
var
  R: TAiEvalCaseResult;
begin
  Result := 0;
  for R in FResults do
    if R.Passed then
      Inc(Result);
end;

function TAiEvalReport.GetFailed: Integer;
begin
  Result := FResults.Count - GetPassed;
end;

function TAiEvalReport.AllPassed: Boolean;
begin
  Result := (FResults.Count > 0) and (GetFailed = 0);
end;

function TAiEvalReport.ToText: string;
var
  SB: TStringBuilder;
  R: TAiEvalCaseResult;
begin
  SB := TStringBuilder.Create;
  try
    for R in FResults do
    begin
      if R.Passed then
        SB.Append('PASS  ')
      else
        SB.Append('FAIL  ');
      SB.Append(R.CaseName).Append(' (').Append(R.DurationMs).Append(' ms)');
      if not R.Passed then
        SB.Append(' — ').Append(R.FailReason);
      SB.AppendLine;
    end;
    SB.Append(Format('%d/%d PASS', [GetPassed, FResults.Count]));
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function TAiEvalReport.ToJSON: TJSONObject;
var
  Arr: TJSONArray;
  R: TAiEvalCaseResult;
  Item: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('total', TJSONNumber.Create(FResults.Count));
  Result.AddPair('passed', TJSONNumber.Create(GetPassed));
  Result.AddPair('failed', TJSONNumber.Create(GetFailed));
  Arr := TJSONArray.Create;
  Result.AddPair('cases', Arr);
  for R in FResults do
  begin
    Item := TJSONObject.Create;
    Item.AddPair('name', R.CaseName);
    Item.AddPair('passed', TJSONBool.Create(R.Passed));
    Item.AddPair('durationMs', TJSONNumber.Create(R.DurationMs));
    if not R.Passed then
    begin
      Item.AddPair('failReason', R.FailReason);
      // La salida real: sin ella el reporte dice que fallo pero no que se
      // obtuvo, y hay que reproducir el caso a mano para averiguarlo.
      Item.AddPair('actual', R.Actual);
    end;
    Arr.AddElement(Item);
  end;
end;

{ TAiEvalRunner }

constructor TAiEvalRunner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCases := TObjectList<TAiEvalCase>.Create(True);
end;

destructor TAiEvalRunner.Destroy;
begin
  FCases.Free;
  inherited;
end;

procedure TAiEvalRunner.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FJudge) then
    FJudge := nil;
end;

procedure TAiEvalRunner.SetJudge(const Value: TAiChat);
begin
  if FJudge <> Value then
  begin
    FJudge := Value;
    if Assigned(FJudge) then
      FJudge.FreeNotification(Self);
  end;
end;

function TAiEvalRunner.AddCase(const AName: string): TAiEvalCase;
begin
  Result := TAiEvalCase.Create(AName);
  FCases.Add(Result);
end;

procedure TAiEvalRunner.Clear;
begin
  FCases.Clear;
end;

// LLM-as-judge: pregunta al chat si la salida cumple el criterio. La
// respuesta debe empezar por PASS o FAIL (prompt estricto); cualquier otra
// cosa se considera fallo del juicio (conservador).
function TAiEvalRunner.RunJudge(const ACriteria, AActual: string; out AReason: string): Boolean;
var
  Prompt, Resp: string;
begin
  Result := False;
  AReason := '';
  if not Assigned(FJudge) then
  begin
    AReason := 'judge check requested but no Judge chat assigned';
    Exit;
  end;

  Prompt :=
    'Eres un evaluador estricto de calidad. Evalua si la RESPUESTA cumple el CRITERIO.' + sLineBreak +
    'CRITERIO: ' + ACriteria + sLineBreak +
    'RESPUESTA: """' + AActual + '"""' + sLineBreak +
    'Contesta UNICAMENTE con una palabra: PASS si cumple, FAIL si no cumple.';

  Resp := Trim(FJudge.AddMessageAndRun(Prompt, 'user', []));
  if StartsText('PASS', Resp) then
    Result := True
  else
    AReason := Format('judge says: %s (criteria: %s)', [Copy(Resp, 1, 120), ACriteria]);
end;

function TAiEvalRunner.EvaluateCase(ACase: TAiEvalCase; const AActual: string; out AReason: string): Boolean;
var
  C: TAiEvalCheck;
  JudgeReason: string;
begin
  Result := True;
  AReason := '';
  for C in ACase.FChecks do
  begin
    case C.Kind of
      ekContains:
        if not AActual.ToLower.Contains(C.Value.ToLower) then
        begin
          Result := False;
          AReason := Format('expected to contain "%s"', [C.Value]);
        end;
      ekNotContains:
        if AActual.ToLower.Contains(C.Value.ToLower) then
        begin
          Result := False;
          AReason := Format('expected NOT to contain "%s"', [C.Value]);
        end;
      ekRegex:
        if not TRegEx.IsMatch(AActual, C.Value) then
        begin
          Result := False;
          AReason := Format('expected to match regex "%s"', [C.Value]);
        end;
      ekEquals:
        if not SameText(Trim(AActual), Trim(C.Value)) then
        begin
          Result := False;
          AReason := Format('expected exact value "%s"', [C.Value]);
        end;
      ekMinLength:
        if Length(AActual) < StrToIntDef(C.Value, 0) then
        begin
          Result := False;
          AReason := Format('expected at least %s chars, got %d', [C.Value, Length(AActual)]);
        end;
      ekMaxLength:
        if Length(AActual) > StrToIntDef(C.Value, MaxInt) then
        begin
          Result := False;
          AReason := Format('expected at most %s chars, got %d', [C.Value, Length(AActual)]);
        end;
      ekJudge:
        if not RunJudge(C.Value, AActual, JudgeReason) then
        begin
          Result := False;
          AReason := JudgeReason;
        end;
    end;
    if not Result then
      Break; // primer check fallido detiene el caso
  end;
end;

function TAiEvalRunner.Run(ATarget: TAiEvalTargetFunc): TAiEvalReport;
var
  ACase: TAiEvalCase;
  CaseResult: TAiEvalCaseResult;
  Actual, Reason: string;
  SW: TStopwatch;
  LSpan: TAiSpan;
begin
  if not Assigned(ATarget) then
    raise Exception.Create('TAiEvalRunner.Run: target no asignado');

  Result := TAiEvalReport.Create;
  for ACase in FCases do
  begin
    CaseResult := TAiEvalCaseResult.Create;
    CaseResult.CaseName := ACase.FName;
    Result.Results.Add(CaseResult);

    LSpan := AiSpanStart('eval.case ' + ACase.FName);
    AiSpanAttr(LSpan, 'eval.case.name', ACase.FName);
    SW := TStopwatch.StartNew;
    try
      Actual := ATarget(ACase.FInput);
      CaseResult.Passed := EvaluateCase(ACase, Actual, Reason);
      CaseResult.FailReason := Reason;
    except
      on E: Exception do
      begin
        Actual := '';
        CaseResult.Passed := False;
        CaseResult.FailReason := 'target exception: ' + E.Message;
      end;
    end;
    CaseResult.DurationMs := SW.ElapsedMilliseconds;
    CaseResult.Actual := Copy(Actual, 1, MAX_ACTUAL_STORED);

    AiSpanAttr(LSpan, 'eval.passed', CaseResult.Passed);
    if CaseResult.Passed then
      AiSpanEnd(LSpan)
    else
      AiSpanEnd(LSpan, CaseResult.FailReason);
  end;
end;

end.
