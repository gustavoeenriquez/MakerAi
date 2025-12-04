unit uMakerAi.Utils.Python;


// IT License
//
// Copyright (c) <year> <copyright holders>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// o use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// HE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// Nombre: Gustavo Enríquez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/


{
  ===============================================================================
  uMakerAi.Utils.Python - Utilidad para ejecutar scripts Python desde Delphi
  ===============================================================================

  Esta librería proporciona una interfaz simple y eficiente para ejecutar
  scripts de Python desde aplicaciones Delphi, permitiendo aprovechar el
  poder de Python para cálculos complejos, análisis de datos, inteligencia
  artificial y más.

  CARACTERÍSTICAS PRINCIPALES:
  • Ejecución de scripts Python con y sin parámetros
  • Conversión automática de tipos Delphi a Python
  • Manejo seguro de memoria y referencias
  • Soporte para múltiples tipos de datos (String, Integer, Float, Boolean, DateTime)
  • Motor Python global reutilizable para mejor rendimiento

  REQUISITOS:
  • Python 3.10 o superior instalado en el sistema
  • Componente Python4Delphi (P4D)
  • DLL de Python accesible (python310.dll por defecto)

  USO BÁSICO:

  1. Sin parámetros:
  Result := TUtilsPython.ExecuteScript('result = "Hello from Python!"');

  2. Con parámetros:
  Result := TUtilsPython.ExecuteScript('result = param1 + param2', [10, 20]);

  EJEMPLO PRÁCTICO - Cálculo de Números Primos:

  var
  ScriptPrimos: String;
  Resultado: String;
  begin
  ScriptPrimos :=
  'def primos(n):' + sLineBreak +
  '    primos_list = []' + sLineBreak +
  '    num = 2' + sLineBreak +
  '    while len(primos_list) < n:' + sLineBreak +
  '        es_primo = True' + sLineBreak +
  '        for i in range(2, int(num ** 0.5) + 1):' + sLineBreak +
  '            if num % i == 0:' + sLineBreak +
  '                es_primo = False' + sLineBreak +
  '                break' + sLineBreak +
  '        if es_primo:' + sLineBreak +
  '            primos_list.append(num)' + sLineBreak +
  '        num += 1' + sLineBreak +
  '    return primos_list' + sLineBreak + sLineBreak +
  'result = primos(param1)';

  // Calcular los primeros 10 números primos
  Resultado := TUtilsPython.ExecuteScript(ScriptPrimos, [10]);
  ShowMessage(Resultado); // Resultado: [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
  end;

  OTROS EJEMPLOS:

  • Operaciones matemáticas:
  Result := TUtilsPython.ExecuteScript('result = param1 ** param2', [2, 8]); // 256

  • Manipulación de strings:
  Result := TUtilsPython.ExecuteScript(
  'result = param1.upper() + " " + param2.lower()',
  ['Hello', 'WORLD']
  ); // "HELLO world"

  • Trabajo con fechas:
  Result := TUtilsPython.ExecuteScript(
  'result = param1.strftime("%Y-%m-%d %H:%M:%S")',
  [Now]
  );


  NOTAS IMPORTANTES:
  • Todos los scripts deben asignar su resultado a la variable 'result'
  • Los parámetros se pasan como param1, param2, param3, etc.
  • El motor Python se inicializa automáticamente
  • Se recomienda manejar excepciones al ejecutar scripts complejos

  AUTOR: Gustavo Enríquez
  VERSIÓN: 1.1 - Soporte para parámetros dinámicos
  FECHA: 2024

  ===============================================================================

  ADVERTENCI!  Asegurese que la aplicación esté en 64 bits si tiene instalado Python en architectura 64 bits.

}

interface

uses
  PythonEngine, System.SysUtils, System.Variants;

Type

  EPythonArchitectureError = class(Exception);

  TUtilsPython = Class
  Private
    class function VariantToPythonValue(const Value: Variant): String;
    class procedure CheckArchitecture;
    class function Is64BitApplication: Boolean;
    class function GetApplicationArchitecture: String;
  Protected
  Public
    class function ExecuteScript(Script: String): String; overload;
    class function ExecuteScript(Script: String; const Params: array of Variant): String; overload;
  End;

var
  GlPythonEngine: TPythonEngine;

procedure InitializePythonEngine;
Function GetPythonEngine(DllName: String = 'python310.dll'): TPythonEngine;

implementation

Function GetPythonEngine(DllName: String): TPythonEngine;
Begin
  if not Assigned(GlPythonEngine) then
  begin
    GlPythonEngine := TPythonEngine.Create(nil);
  end;

  // Verificar arquitectura antes de inicializar
  TUtilsPython.CheckArchitecture;

  GlPythonEngine.DllName := DllName; // Asegúrate de usar la versión correcta de Python

  If Not GlPythonEngine.Initialized then
    GlPythonEngine.LoadDll;

  Result := GlPythonEngine;
End;

procedure InitializePythonEngine;
begin
  if not Assigned(GlPythonEngine) then
  begin
    GlPythonEngine := TPythonEngine.Create(nil);
  end;
end;

{ TPythonUtils }

class function TUtilsPython.Is64BitApplication: Boolean;
begin
{$IFDEF WIN64}
  Result := True;
{$ELSE}
  Result := False;
{$ENDIF}
end;

class function TUtilsPython.GetApplicationArchitecture: String;
begin
  if Is64BitApplication then
    Result := '64 bits'
  else
    Result := '32 bits';
end;

class procedure TUtilsPython.CheckArchitecture;
begin
  if not Is64BitApplication then
  begin
    raise EPythonArchitectureError.CreateFmt('ERROR DE ARQUITECTURA: La aplicación está compilada en %s pero Python generalmente se instala en 64 bits.' + sLineBreak + 'SOLUCIONES:' + sLineBreak +
      '1. Recompile la aplicación para 64 bits (Recomendado)' + sLineBreak + '2. Instale Python 32 bits desde https://www.python.org/downloads/' + sLineBreak + '3. Use la DLL de Python 32 bits (python310-32.dll)' + sLineBreak + sLineBreak +
      'Para compilar en 64 bits:' + sLineBreak + '- En Delphi: Project > Options > Building > Delphi Compiler > Target platforms > Win64' + sLineBreak + '- Seleccione "Win64" como plataforma de destino', [GetApplicationArchitecture]);
  end;
end;

class function TUtilsPython.ExecuteScript(Script: String): String;
// En esta función el script debe retornar el resultado en una variable llamada result
var
  PyEngine: TPythonEngine;
  PyModule: TPythonModule;
  PyResult: PPyObject;
  FullScript: String;
  UTF8Script: UTF8String;
begin

// Verificar arquitectura antes de ejecutar
  CheckArchitecture;

  PyEngine := GetPythonEngine; // Obtiene el engine global

  try
    // Crear el módulo de Python
    PyModule := TPythonModule.Create(nil);
    PyModule.Engine := PyEngine;
    PyModule.ModuleName := 'mymodule';

    FullScript := '# -*- coding: utf-8 -*-' + sLineBreak + Script;

    // Convertir a UTF-8
    UTF8Script := UTF8Encode(FullScript);

    // Ejecutar el script completo de Python
    PyEngine.ExecString(AnsiString(UTF8Script));

    // Obtener el resultado de la ejecución
    PyResult := PyEngine.EvalString('result');

    // Convertir el resultado a una cadena
    Result := PyEngine.PyObjectAsString(PyResult);

    // Decrementar la referencia del objeto Python
    PyEngine.Py_DecRef(PyResult);
  Except
    On E: Exception do
    Begin
      Result := E.Message + sLineBreak + 'Recuerda que todos los script deben retornar un valor en una variable llamada Result';
      Raise;
    End;
  end;
end;

class function TUtilsPython.ExecuteScript(Script: String; const Params: array of Variant): String;
var
  PyEngine: TPythonEngine;
  PyModule: TPythonModule;
  PyResult: PPyObject;
  ParameterScript: String;
  i: Integer;
  FullScript: String;
  UTF8Script: UTF8String;
begin

// Verificar arquitectura antes de ejecutar
  CheckArchitecture;

  PyEngine := GetPythonEngine;
  try
    // Crear el módulo de Python
    PyModule := TPythonModule.Create(nil);
    try
      PyModule.Engine := PyEngine;
      PyModule.ModuleName := 'mymodule';

      // Construir el script de parámetros
      ParameterScript := '';
      for i := 0 to High(Params) do
      begin
        ParameterScript := ParameterScript + Format('param%d = %s', [i + 1, VariantToPythonValue(Params[i])]) + sLineBreak;
      end;

      // Si hay parámetros de tipo fecha, agregar import de datetime
      for i := 0 to High(Params) do
      begin
        if VarType(Params[i]) = varDate then
        begin
          ParameterScript := 'import datetime' + sLineBreak + ParameterScript;
          Break;
        end;
      end;

      // Agregar encoding UTF-8 al inicio del script
      FullScript := '# -*- coding: utf-8 -*-' + sLineBreak + ParameterScript + sLineBreak + Script;

      // Convertir a UTF-8
      UTF8Script := UTF8Encode(FullScript);

      // Ejecutar el script completo de Python
      PyEngine.ExecString(AnsiString(UTF8Script));

      // Obtener el resultado de la ejecución
      PyResult := PyEngine.EvalString('result');
      try
        // Convertir el resultado a una cadena
        Result := PyEngine.PyObjectAsString(PyResult);
      finally
        // Decrementar la referencia del objeto Python
        PyEngine.Py_DecRef(PyResult);
      end;
    finally
      PyModule.Free;
    end;
  Except
    On E: Exception do
    Begin
      Result := E.Message + sLineBreak + 'Recuerda que todos los script deben retornar un valor en una variable llamada result';
      Raise;
    End;
  end;
end;

class function TUtilsPython.VariantToPythonValue(const Value: Variant): String;
begin
  case VarType(Value) of
    varEmpty, varNull:
      Result := 'None';
    varBoolean:
      if Value then
        Result := 'True'
      else
        Result := 'False';
    varByte, varSmallint, varInteger, varWord, varLongWord, varInt64, varUInt64:
      Result := VarToStr(Value);
    varSingle, varDouble, varCurrency:
      Result := StringReplace(VarToStr(Value), ',', '.', [rfReplaceAll]); // Formato decimal para Python
    varDate:
      Result := Format('datetime.datetime(%s)', [FormatDateTime('yyyy,m,d,h,n,s', Value)]);
    varString, varUString, varOleStr:
      Result := QuotedStr(VarToStr(Value));
  else
    Result := QuotedStr(VarToStr(Value)); // Por defecto como string
  end;
end;

initialization

InitializePythonEngine;

finalization

FreeAndNil(GlPythonEngine);

end.



 def primos(n):
     primos_list = []
     num = 2
     while len(primos_list) < n:
         es_primo = True
         for i in range(2, int(num ** 0.5) + 1):
             if num % i == 0:
                 es_primo = False
                 break
         if es_primo:
             primos_list.append(num)
         num += 1
     return primos_list
 result = primos(param1)

