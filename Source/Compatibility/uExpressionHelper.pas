// =========================================================================
// uExpressionHelper.pas - Motor de Expresiones Multiplataforma
// =========================================================================
// Helper que encapsula la evaluación de expresiones booleanas para
// mantener paridad entre Delphi (LiveBindings) y FPC (fpexprparser).
//
// Uso:
//   Result := EvaluateExpression('count > 5 and status = ''completed''', AVars);
//
// Operadores soportados:
//   Comparación: =, <>, >, <, >=, <=
//   Lógicos: and, or, not
//   Aritméticos: +, -, *, /
// =========================================================================
unit uExpressionHelper;

{$INCLUDE ../CompilerDirectives.inc}

interface

uses
  {$IFDEF FPC}
  SysUtils, Classes, TypInfo, Generics.Collections, Rtti,
  fpexprpars,
  {$ELSE}
  System.SysUtils, System.Classes, System.TypInfo,
  System.Generics.Collections, System.Rtti,
  System.Bindings.Evaluator, System.Bindings.Helper,
  System.Bindings.Expression, System.Bindings.EvalProtocol,
  System.Bindings.Consts, System.Bindings.EvalSys, 
  System.Bindings.Factories, System.Bindings.ObjEval,
  {$ENDIF}
  uRttiHelper;

/// <summary>
/// Evalúa una expresión booleana con variables de contexto.
/// </summary>
/// <param name="AExpr">Expresión a evaluar (ej: "count > 5")</param>
/// <param name="AVars">Diccionario de variables con sus valores</param>
/// <returns>Resultado booleano de la expresión</returns>
/// <remarks>
/// En FPC usa fpexprparser (nativo FCL).
/// En Delphi usa LiveBindings (System.Bindings).
/// Si la expresión es inválida o vacía, retorna False.
/// </remarks>
function EvaluateExpression(const AExpr: string;
  AVars: TDictionary<string, TValue>): Boolean;

implementation

{$IFDEF FPC}
// =========================================================================
// Implementación FPC usando fpexprparser
// =========================================================================
function EvaluateExpression(const AExpr: string;
  AVars: TDictionary<string, TValue>): Boolean;
var
  Parser: TFPExpressionParser;
  Pair: TPair<string, TValue>;
  V: TValue;
  VarName: string;
begin
  Result := False;
  if Trim(AExpr) = '' then
    Exit;

  Parser := TFPExpressionParser.Create(nil);
  try
    try
      // Registrar todas las variables del diccionario en el parser
      for Pair in AVars do
      begin
        V := Pair.Value;
        VarName := Pair.Key;
        
        // Mapear TValue a tipos soportados por fpexprparser
        case V.Kind of
          tkInteger:
            Parser.Identifiers.AddIntegerVariable(VarName, V.AsInteger);
          tkInt64:
            Parser.Identifiers.AddIntegerVariable(VarName, V.AsInt64);
          tkFloat:
            Parser.Identifiers.AddFloatVariable(VarName, V.AsExtended);
          tkString, tkUString, tkLString, tkWString, tkAString:
            Parser.Identifiers.AddStringVariable(VarName, V.AsString);
          tkEnumeration:
            begin
              // Boolean es un caso especial de enumeración
              if V.IsType<Boolean> then
                Parser.Identifiers.AddBooleanVariable(VarName, V.AsBoolean)
              else
                // Otros enums: convertir a integer o string
                Parser.Identifiers.AddIntegerVariable(VarName, V.AsOrdinal);
            end;
        end;
      end;

      // Evaluar la expresión
      Parser.Expression := AExpr;
      Result := Parser.Evaluate.ResBoolean;
    except
      on E: Exception do
      begin
        // Expresión inválida o error de evaluación = False
        // No lanzamos excepción para mantener compatibilidad con el comportamiento
        // esperado en el motor de agentes (fallback a NextNo)
        Result := False;
      end;
    end;
  finally
    Parser.Free;
  end;
end;

{$ELSE}
// =========================================================================
// Implementación Delphi usando LiveBindings
// =========================================================================
function EvaluateExpression(const AExpr: string;
  AVars: TDictionary<string, TValue>): Boolean;
var
  LScope: IScope;
  LDictScope: TDictionaryScope;
  BindingExpression: TBindingExpression;
  Pair: TPair<string, TValue>;
  Value: TValue;
  ValueWrapper: IValue;
begin
  Result := False;
  if Trim(AExpr) = '' then
    Exit;

  // Crear scope con interfaz para gestión automática de memoria
  LDictScope := TDictionaryScope.Create;
  LScope := LDictScope;

  // Llenar las variables usando la referencia de clase
  for Pair in AVars do
  begin
    ValueWrapper := TValueWrapper.Create(Pair.Value);
    LDictScope.Map.Add(Pair.Key, ValueWrapper);
  end;

  // Crear y evaluar la expresión
  BindingExpression := TBindings.CreateExpression([LScope], AExpr);
  try
    ValueWrapper := BindingExpression.Evaluate;
    Value := ValueWrapper.GetValue;

    if Value.IsType<Boolean> then
      Result := Value.AsBoolean
    else
      raise Exception.CreateFmt('La expresión "%s" no devolvió un Boolean', [AExpr]);
  finally
    BindingExpression.Free;
  end;
end;
{$ENDIF}

end.

