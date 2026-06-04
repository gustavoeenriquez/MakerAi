unit uCalculatorTool;

interface

uses
  System.SysUtils, System.Math, System.JSON,
  uMakerAi.MCPServer.Core;

type
  TCalculateParams = class
  private
    FExpression: string;
  public
    [AiMCPSchemaDescription('Mathematical expression to evaluate. Supports: +,-,*,/,^,sqrt(),abs(),floor(),ceil(),round(),sin(),cos(),tan(),log10(),ln(),exp(),pi,e. Example: "2+3*4" or "sqrt(16)+2^3"')]
    property Expression: string read FExpression write FExpression;
  end;

  TCalculatorTool = class(TAiMCPToolBase<TCalculateParams>)
  protected
    function ExecuteWithParams(const AParams: TCalculateParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

implementation

// -------------------------------------------------------------------------
// Recursive descent expression parser
// Grammar:
//   expr    -> term   ( ('+' | '-') term   )*
//   term    -> factor ( ('*' | '/' | '%') factor )*
//   factor  -> primary ('^' factor)?        (right-associative)
//   primary -> NUMBER | '(' expr ')' | '-' factor | '+' factor | IDENT
//   IDENT   -> function_call | constant
// -------------------------------------------------------------------------
type
  TExprParser = class
  private
    FExpr: string;
    FPos:  Integer;
    function  Peek: Char;
    procedure Skip;
    procedure SkipSpaces;
    function  ParseExpr: Double;
    function  ParseTerm: Double;
    function  ParseFactor: Double;
    function  ParsePrimary: Double;
    function  ParseNumber: Double;
    function  ParseIdent: string;
    function  IsDigit(C: Char): Boolean; inline;
    function  IsAlpha(C: Char): Boolean; inline;
  public
    function Evaluate(const AExpr: string): Double;
  end;

function TExprParser.Peek: Char;
begin
  if FPos <= Length(FExpr) then
    Result := FExpr[FPos]
  else
    Result := #0;
end;

procedure TExprParser.Skip;
begin
  Inc(FPos);
end;

procedure TExprParser.SkipSpaces;
begin
  while (FPos <= Length(FExpr)) and (FExpr[FPos] = ' ') do
    Inc(FPos);
end;

function TExprParser.IsDigit(C: Char): Boolean;
begin
  Result := (C >= '0') and (C <= '9');
end;

function TExprParser.IsAlpha(C: Char): Boolean;
begin
  Result := ((C >= 'a') and (C <= 'z')) or
            ((C >= 'A') and (C <= 'Z')) or
            (C = '_');
end;

function TExprParser.ParseNumber: Double;
var
  S: string;
  FS: TFormatSettings;
begin
  S := '';
  while IsDigit(Peek) or (Peek = '.') do
  begin
    S := S + Peek;
    Skip;
  end;
  FS := TFormatSettings.Create('en-US');
  Result := StrToFloat(S, FS);
end;

function TExprParser.ParseIdent: string;
begin
  Result := '';
  while IsAlpha(Peek) or IsDigit(Peek) do
  begin
    Result := Result + Peek;
    Skip;
  end;
  Result := LowerCase(Result);
end;

function TExprParser.ParsePrimary: Double;
var
  Ident: string;
  V: Double;
begin
  SkipSpaces;
  if Peek = #0 then
    raise Exception.Create('Unexpected end of expression');

  if Peek = '(' then
  begin
    Skip; // '('
    Result := ParseExpr;
    SkipSpaces;
    if Peek <> ')' then
      raise Exception.Create('Missing closing parenthesis');
    Skip; // ')'
  end
  else if Peek = '-' then
  begin
    Skip;
    Result := -ParseFactor;
  end
  else if Peek = '+' then
  begin
    Skip;
    Result := ParseFactor;
  end
  else if IsDigit(Peek) or (Peek = '.') then
    Result := ParseNumber
  else if IsAlpha(Peek) then
  begin
    Ident := ParseIdent;
    SkipSpaces;
    // Constants
    if Ident = 'pi' then
      Result := Pi
    else if Ident = 'e' then
      Result := Exp(1)
    else
    begin
      // Function call — expect '('
      if Peek <> '(' then
        raise Exception.CreateFmt('Unknown constant or missing "(": "%s"', [Ident]);
      Skip; // '('
      V := ParseExpr;
      SkipSpaces;
      if Peek <> ')' then
        raise Exception.Create('Missing closing parenthesis after function argument');
      Skip; // ')'
      if      Ident = 'sqrt'  then Result := Sqrt(V)
      else if Ident = 'abs'   then Result := Abs(V)
      else if Ident = 'floor' then Result := Floor(V)
      else if Ident = 'ceil'  then Result := Ceil(V)
      else if Ident = 'round' then Result := RoundTo(V, 0)
      else if Ident = 'sin'   then Result := Sin(V)
      else if Ident = 'cos'   then Result := Cos(V)
      else if Ident = 'tan'   then Result := Tan(V)
      else if (Ident = 'log') or (Ident = 'log10') then Result := Log10(V)
      else if Ident = 'ln'    then Result := Ln(V)
      else if Ident = 'exp'   then Result := Exp(V)
      else if Ident = 'sgn'   then
        if V > 0 then Result := 1 else if V < 0 then Result := -1 else Result := 0
      else
        raise Exception.CreateFmt('Unknown function: "%s"', [Ident]);
    end;
  end
  else
    raise Exception.CreateFmt('Unexpected character "%s" at position %d', [Peek, FPos]);
end;

function TExprParser.ParseFactor: Double;
var
  Base: Double;
begin
  Base := ParsePrimary;
  SkipSpaces;
  if Peek = '^' then
  begin
    Skip;
    Result := Power(Base, ParseFactor); // right-associative
  end
  else
    Result := Base;
end;

function TExprParser.ParseTerm: Double;
var
  Op: Char;
  Right: Double;
begin
  Result := ParseFactor;
  while True do
  begin
    SkipSpaces;
    Op := Peek;
    if (Op <> '*') and (Op <> '/') and (Op <> '%') then Break;
    Skip;
    Right := ParseFactor;
    case Op of
      '*': Result := Result * Right;
      '/': begin
             if Right = 0 then raise Exception.Create('Division by zero');
             Result := Result / Right;
           end;
      '%': begin
             if Right = 0 then raise Exception.Create('Modulo by zero');
             Result := System.Math.FMod(Result, Right);
           end;
    end;
  end;
end;

function TExprParser.ParseExpr: Double;
var
  Op: Char;
  Right: Double;
begin
  Result := ParseTerm;
  while True do
  begin
    SkipSpaces;
    Op := Peek;
    if (Op <> '+') and (Op <> '-') then Break;
    Skip;
    Right := ParseTerm;
    if Op = '+' then Result := Result + Right
    else             Result := Result - Right;
  end;
end;

function TExprParser.Evaluate(const AExpr: string): Double;
begin
  FExpr := Trim(AExpr);
  FPos  := 1;
  Result := ParseExpr;
  SkipSpaces;
  if FPos <= Length(FExpr) then
    raise Exception.CreateFmt('Unexpected character "%s" at position %d', [Peek, FPos]);
end;

// -------------------------------------------------------------------------
// TCalculatorTool
// -------------------------------------------------------------------------

constructor TCalculatorTool.Create;
begin
  inherited;
  FName        := 'calculate';
  FDescription := 'Evaluate a mathematical expression. Supports arithmetic operators (+,-,*,/,%), ' +
                  'exponentiation (^), grouping (parentheses), functions (sqrt, abs, floor, ceil, ' +
                  'round, sin, cos, tan, log10, ln, exp) and constants (pi, e).';
end;

function TCalculatorTool.ExecuteWithParams(const AParams: TCalculateParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  Parser: TExprParser;
  Value:  Double;
  Fmt:    TFormatSettings;
begin
  if Trim(AParams.Expression) = '' then
    raise Exception.Create('Expression cannot be empty');

  Parser := TExprParser.Create;
  try
    Value := Parser.Evaluate(Trim(AParams.Expression));
  finally
    Parser.Free;
  end;

  // Format: strip unnecessary trailing zeros
  Fmt := TFormatSettings.Create('en-US');
  if Frac(Value) = 0 then
    Result := TAiMCPResponseBuilder.New.AddText(IntToStr(Round(Value))).Build
  else
    Result := TAiMCPResponseBuilder.New.AddText(FloatToStrF(Value, ffGeneral, 15, 0, Fmt)).Build;
end;

end.
