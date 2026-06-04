unit uDmW01Calculator;

interface

uses
  System.SysUtils,
  System.Classes,
  uMakerAi.Agents,
  uMakerAi.Agents.Node.LLM;

type
  TDmW01 = class(TDataModule)
    Manager    : TAIAgentManager;
    NodeParser : TLLMNode;
    LinkPC     : TAIAgentsLink;
    LinkCF     : TAIAgentsLink;
    NodeFmt    : TLLMNode;
    procedure DataModuleCreate(Sender: TObject);
    procedure DoCalculate(Node, BeforeNode: TAIAgentsNode;
      Link: TAIAgentsLink; Input: String; var Output: String);
  private
    FNodeCalc  : TAIAgentsNode;
  public
    procedure RunDemo;
  end;

var
  DmW01: TDmW01;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

uses
  System.Math;

// =============================================================================
// TExprCalc — evaluador aritmetico de expresiones de texto
// Soporta: +  -  *  /  ()  sqrt()  numeros decimales
// =============================================================================
type
  TExprCalc = class
  private
    FExpr : string;
    FPos  : Integer;
    procedure SkipSpaces;
    function  Current: Char;
    function  IsEnd: Boolean;
    function  ParseNumber: Double;
    function  ParseFactor: Double;
    function  ParseTerm: Double;
    function  ParseAddSub: Double;
  public
    class function Evaluate(const AExpr: string): Double;
  end;

procedure TExprCalc.SkipSpaces;
begin
  while (FPos <= Length(FExpr)) and (FExpr[FPos] = ' ') do
    Inc(FPos);
end;

function TExprCalc.Current: Char;
begin
  if FPos <= Length(FExpr) then
    Result := FExpr[FPos]
  else
    Result := #0;
end;

function TExprCalc.IsEnd: Boolean;
begin
  Result := FPos > Length(FExpr);
end;

function TExprCalc.ParseNumber: Double;
var
  Start : Integer;
  S     : String;
  Neg   : Boolean;
begin
  SkipSpaces;
  Neg := False;
  if Current = '-' then
  begin
    Neg := True;
    Inc(FPos);
    SkipSpaces;
  end;
  Start := FPos;
  while (not IsEnd) and (Current in ['0'..'9', '.']) do
    Inc(FPos);
  S := Copy(FExpr, Start, FPos - Start);
  if S = '' then
    Result := 0
  else
    Result := StrToFloat(S, TFormatSettings.Invariant);
  if Neg then
    Result := -Result;
end;

function TExprCalc.ParseFactor: Double;
const
  SQRT_TOKEN = 'sqrt';
begin
  SkipSpaces;
  if Current = '(' then
  begin
    Inc(FPos);  // skip '('
    Result := ParseAddSub;
    SkipSpaces;
    if Current = ')' then Inc(FPos);  // skip ')'
  end
  else if (FPos + Length(SQRT_TOKEN) - 1 <= Length(FExpr)) and
          (LowerCase(Copy(FExpr, FPos, Length(SQRT_TOKEN))) = SQRT_TOKEN) then
  begin
    Inc(FPos, Length(SQRT_TOKEN));  // skip 'sqrt'
    SkipSpaces;
    if Current = '(' then Inc(FPos);
    Result := Sqrt(ParseAddSub);
    SkipSpaces;
    if Current = ')' then Inc(FPos);
  end
  else
    Result := ParseNumber;
end;

function TExprCalc.ParseTerm: Double;
var
  Op    : Char;
  Right : Double;
begin
  Result := ParseFactor;
  SkipSpaces;
  while (not IsEnd) and (Current in ['*', '/']) do
  begin
    Op := Current;
    Inc(FPos);
    Right := ParseFactor;
    if Op = '*' then
      Result := Result * Right
    else if (Op = '/') and (Right <> 0) then
      Result := Result / Right;
    SkipSpaces;
  end;
end;

function TExprCalc.ParseAddSub: Double;
var
  Op    : Char;
  Right : Double;
begin
  Result := ParseTerm;
  SkipSpaces;
  while (not IsEnd) and (Current in ['+', '-']) do
  begin
    Op := Current;
    Inc(FPos);
    Right := ParseTerm;
    if Op = '+' then
      Result := Result + Right
    else
      Result := Result - Right;
    SkipSpaces;
  end;
end;

class function TExprCalc.Evaluate(const AExpr: string): Double;
var
  Calc: TExprCalc;
begin
  Calc := TExprCalc.Create;
  try
    Calc.FExpr := Trim(AExpr);
    Calc.FPos  := 1;
    Result := Calc.ParseAddSub;
  finally
    Calc.Free;
  end;
end;

// =============================================================================
// DataModule
// =============================================================================

procedure TDmW01.DataModuleCreate(Sender: TObject);
begin
  // NodeCalc se crea en codigo porque TAIAgentsNode no tiene componente de paleta
  FNodeCalc           := TAIAgentsNode.Create(Manager);
  FNodeCalc.Name      := 'Calculator';
  FNodeCalc.Graph     := Manager;
  FNodeCalc.OnExecute := DoCalculate;
  FNodeCalc.Next      := LinkCF;
  LinkPC.NextA        := FNodeCalc;

  // Asegurar el grafo (pueden estar ya en el DFM; asignar aqui es inofensivo)
  NodeParser.Next     := LinkPC;
  Manager.StartNode   := NodeParser;
  Manager.EndNode     := NodeFmt;
  Manager.Compile;
end;

procedure TDmW01.DoCalculate(Node, BeforeNode: TAIAgentsNode;
  Link: TAIAgentsLink; Input: String; var Output: String);
var
  Expr   : String;
  Value  : Double;
  FmtVal : String;
begin
  Expr := Trim(Input);
  try
    Value  := TExprCalc.Evaluate(Expr);
    // Mostrar sin decimales innecesarios
    if Frac(Value) = 0 then
      FmtVal := IntToStr(Round(Value))
    else
      FmtVal := FormatFloat('0.####', Value);
    Output := Expr + ' = ' + FmtVal;
    Writeln(Format('  [Calculator] %s', [Output]));
  except
    on E: Exception do
    begin
      Output := 'Error evaluating "' + Expr + '": ' + E.Message;
      Writeln('  [Calculator] ERROR: ', E.Message);
    end;
  end;
end;

procedure TDmW01.RunDemo;

  procedure Solve(const AQuestion: String);
  begin
    Writeln(StringOfChar('-', 60));
    Writeln('Pregunta : ', AQuestion);
    Writeln('Respuesta: ', Manager.Run(AQuestion));
    Writeln;
  end;

begin
  Writeln('=== W01 - SimpleCalculator ===');
  Writeln('Pipeline: Parser(LLM) -> Calculator(OnExecute) -> Formatter(LLM)');
  Writeln('Componentes configurados en tiempo de diseno (DataModule).');
  Writeln;

  Solve('What is 15 percent of 240?');
  Solve('If a shirt costs $45 and is 30% off, what is the final price?');
  Solve('What is the square root of 144 plus 7 times 3?');
  Solve('A pizza costs $12.50. If I order 4 pizzas with a 20% discount, what do I pay?');
end;

end.
