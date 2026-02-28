// MIT License - Copyright (c) 2024-2026 Gustavo Enriquez
// Demo: TAiFunctions — registro de funciones y serialización multi-formato
// Prueba: add functions, serialize to OpenAI/Claude/Gemini JSON, DoCallFunction.
program demo_functions;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fpjson, jsonparser,
  uMakerAi.Core,
  uMakerAi.Chat.Messages,
  uMakerAi.Tools.Functions;

// ---------------------------------------------------------------------------
// Clase que contiene los handlers (TFunctionEvent = of object)
// ---------------------------------------------------------------------------

type
  TFunctionHandlers = class
    procedure OnGetWeather(Sender: TObject; FunctionAction: TFunctionActionItem;
        FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure OnCalculate(Sender: TObject; FunctionAction: TFunctionActionItem;
        FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
  end;

procedure TFunctionHandlers.OnGetWeather(Sender: TObject;
    FunctionAction: TFunctionActionItem; FunctionName: string;
    ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  Parsed: TJSONData;
  Location, Unit_: string;
begin
  Handled := True;
  Location := 'desconocido';
  Unit_ := 'celsius';
  Parsed := GetJSON(ToolCall.Arguments);
  try
    if (Parsed <> nil) and (Parsed is TJSONObject) then
    begin
      Location := TJSONObject(Parsed).Get('location', 'desconocido');
      Unit_    := TJSONObject(Parsed).Get('unit', 'celsius');
    end;
  finally
    Parsed.Free;
  end;
  ToolCall.Response := '{"temperature":22,"unit":"' + Unit_ +
      '","description":"Soleado","location":"' + Location + '"}';
end;

procedure TFunctionHandlers.OnCalculate(Sender: TObject;
    FunctionAction: TFunctionActionItem; FunctionName: string;
    ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  Parsed: TJSONData;
  A, B, Res: Double;
  Op: string;
begin
  Handled := True;
  A := 0; B := 0; Res := 0; Op := 'add';
  Parsed := GetJSON(ToolCall.Arguments);
  try
    if (Parsed <> nil) and (Parsed is TJSONObject) then
    begin
      A  := TJSONObject(Parsed).Get('a', 0.0);
      B  := TJSONObject(Parsed).Get('b', 0.0);
      Op := TJSONObject(Parsed).Get('operation', 'add');
    end;
  finally
    Parsed.Free;
  end;

  if Op = 'add'      then Res := A + B
  else if Op = 'sub' then Res := A - B
  else if Op = 'mul' then Res := A * B
  else if (Op = 'div') and (B <> 0) then Res := A / B
  else
  begin
    ToolCall.Response := '{"error":"operacion desconocida o division por cero"}';
    Exit;
  end;
  ToolCall.Response := '{"result":' + FloatToStr(Res) + '}';
end;

// ---------------------------------------------------------------------------
// Helpers de presentación
// ---------------------------------------------------------------------------

procedure PrintSep(const Title: string);
begin
  WriteLn('');
  WriteLn('=== ', Title, ' ===');
end;

procedure PrintJSON(const Label_, JSON: string);
var
  Parsed: TJSONData;
begin
  WriteLn('  ', Label_, ':');
  Parsed := GetJSON(JSON);
  try
    if Assigned(Parsed) then
      WriteLn(Parsed.FormatJSON)
    else
      WriteLn('  (JSON inválido: ', JSON, ')');
  finally
    Parsed.Free;
  end;
end;

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------
var
  Handlers: TFunctionHandlers;
  Funcs: TAiFunctions;
  Func: TFunctionActionItem;
  Param: TFunctionParamsItem;
  ToolCall: TAiToolsFunction;
  JsonTools: string;
  Names: TStringList;
  I: Integer;
begin
  WriteLn('==============================================');
  WriteLn('  demo_functions — TAiFunctions runtime test');
  WriteLn('==============================================');

  Handlers := TFunctionHandlers.Create;
  Funcs := TAiFunctions.Create(nil);
  try

    // -----------------------------------------------------------------------
    PrintSep('1. Registrar función get_weather');
    // -----------------------------------------------------------------------
    Func := Funcs.Functions.Add;
    Func.FunctionName := 'get_weather';
    Func.Description.Text := 'Obtiene el clima actual de una ubicación.';
    Func.Enabled := True;
    Func.ToolType := tt_function;
    Func.OnAction := @Handlers.OnGetWeather;

    Param := Func.Parameters.Add;
    Param.Name := 'location';
    Param.ParamType := ptString;
    Param.Description.Text := 'Ciudad o coordenadas (ej: "Buenos Aires")';
    Param.Required := True;

    Param := Func.Parameters.Add;
    Param.Name := 'unit';
    Param.ParamType := ptString;
    Param.Description.Text := 'Unidad de temperatura';
    Param.Enum := 'celsius,fahrenheit';
    Param.Required := False;

    WriteLn('  get_weather registrada. Params: ', Func.Parameters.Count);

    // -----------------------------------------------------------------------
    PrintSep('2. Registrar función calculate');
    // -----------------------------------------------------------------------
    Func := Funcs.Functions.AddFunction('calculate', True, @Handlers.OnCalculate);
    Func.Description.Text := 'Realiza operaciones matemáticas básicas.';
    Func.ToolType := tt_function;

    Param := Func.Parameters.Add;
    Param.Name := 'a';
    Param.ParamType := ptFloat;
    Param.Description.Text := 'Primer operando';
    Param.Required := True;

    Param := Func.Parameters.Add;
    Param.Name := 'b';
    Param.ParamType := ptFloat;
    Param.Description.Text := 'Segundo operando';
    Param.Required := True;

    Param := Func.Parameters.Add;
    Param.Name := 'operation';
    Param.ParamType := ptString;
    Param.Description.Text := 'Operación matemática';
    Param.Enum := 'add,sub,mul,div';
    Param.Required := True;

    WriteLn('  calculate registrada. Total funciones: ', Funcs.Functions.Count);

    // -----------------------------------------------------------------------
    PrintSep('3. Serialización — formato OpenAI');
    // -----------------------------------------------------------------------
    JsonTools := Funcs.GetTools(TToolFormat.tfOpenAI);
    PrintJSON('OpenAI tools', JsonTools);

    // -----------------------------------------------------------------------
    PrintSep('4. Serialización — formato Claude (Anthropic)');
    // -----------------------------------------------------------------------
    JsonTools := Funcs.GetTools(TToolFormat.tfClaude);
    PrintJSON('Claude tools', JsonTools);

    // -----------------------------------------------------------------------
    PrintSep('5. Serialización — formato Gemini');
    // -----------------------------------------------------------------------
    JsonTools := Funcs.GetTools(TToolFormat.tfGemini);
    PrintJSON('Gemini tools', JsonTools);

    // -----------------------------------------------------------------------
    PrintSep('6. Ejecutar get_weather via DoCallFunction');
    // -----------------------------------------------------------------------
    ToolCall := TAiToolsFunction.Create;
    try
      ToolCall.Name := 'get_weather';
      ToolCall.Arguments := '{"location":"Córdoba, Argentina","unit":"celsius"}';
      if Funcs.DoCallFunction(ToolCall) then
      begin
        WriteLn('  [OK] Ejecutada. Response:');
        PrintJSON('response', ToolCall.Response);
      end
      else
        WriteLn('  [FAIL] No se ejecutó');
    finally
      ToolCall.Free;
    end;

    // -----------------------------------------------------------------------
    PrintSep('7. Ejecutar calculate (8 * 7) via DoCallFunction');
    // -----------------------------------------------------------------------
    ToolCall := TAiToolsFunction.Create;
    try
      ToolCall.Name := 'calculate';
      ToolCall.Arguments := '{"a":8,"b":7,"operation":"mul"}';
      if Funcs.DoCallFunction(ToolCall) then
      begin
        WriteLn('  [OK] 8 * 7 = resultado:');
        PrintJSON('response', ToolCall.Response);
      end
      else
        WriteLn('  [FAIL] No se ejecutó');
    finally
      ToolCall.Free;
    end;

    // -----------------------------------------------------------------------
    PrintSep('8. SetFunctionEnable — deshabilitar calculate');
    // -----------------------------------------------------------------------
    Funcs.SetFunctionEnable('calculate', False);
    JsonTools := Funcs.GetTools(TToolFormat.tfOpenAI);
    WriteLn('  Tools con calculate=False (solo debería aparecer get_weather):');
    PrintJSON('tools filtradas', JsonTools);

    // -----------------------------------------------------------------------
    PrintSep('9. ExtractFunctionNames');
    // -----------------------------------------------------------------------
    Funcs.SetFunctionEnable('calculate', True);
    Names := Funcs.ExtractFunctionNames;
    try
      WriteLn('  Funciones registradas: ', Names.Count);
      for I := 0 to Names.Count - 1 do
        WriteLn('    - ', Names[I]);
    finally
      Names.Free;
    end;

    // -----------------------------------------------------------------------
    PrintSep('10. Save/Load to JSON file');
    // -----------------------------------------------------------------------
    Funcs.Functions.SaveToFile('demo_functions_test.json');
    WriteLn('  Guardado en demo_functions_test.json');

    Funcs.Functions.Clear;
    Funcs.Functions.LoadFromFile('demo_functions_test.json');
    WriteLn('  Recargado. Funciones: ', Funcs.Functions.Count);
    if Funcs.Functions.Count > 0 then
      WriteLn('  Función 0: ', Funcs.Functions[0].FunctionName);
    DeleteFile('demo_functions_test.json');

    WriteLn('');
    WriteLn('==============================================');
    WriteLn('  TAiFunctions tests completados.');
    WriteLn('==============================================');

  finally
    Funcs.Free;
    Handlers.Free;
  end;
end.
