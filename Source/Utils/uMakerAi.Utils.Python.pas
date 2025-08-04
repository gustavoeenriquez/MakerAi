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
// - Telegram: +57 3128441700
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.Utils.Python;

interface

uses
  PythonEngine, System.SysUtils;

Type
   TUtilsPython = Class
     Private
     Protected
     Public
     Class function ExecuteScript(Script : String) : String;
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

class function TUtilsPython.ExecuteScript(Script: String): String;
// En esta función el script debe retornar el resultado en una variable llamada result
var
  PyEngine : TPythonEngine;
  PyModule: TPythonModule;
  PyResult: PPyObject;
begin

  PyEngine := GetPythonEngine; //Obtiene el engine global

  try
    // Crear el módulo de Python
    PyModule := TPythonModule.Create(nil);
    PyModule.Engine := PyEngine;
    PyModule.ModuleName := 'mymodule';

    // Ejecutar el script de Python
    PyEngine.ExecString(Script);

    // Obtener el resultado de la ejecución
    PyResult := PyEngine.EvalString('result');

    // Convertir el resultado a una cadena
    Result := PyEngine.PyObjectAsString(PyResult);

    // Decrementar la referencia del objeto Python
    PyEngine.Py_DecRef(PyResult);
  Except
    On E: Exception do
    Begin
      Result := E.Message +sLineBreak+'Recuerda que todos los script deben retornar algo en una variable llamada Result';
      Raise;
    End;
  end;
end;

initialization

InitializePythonEngine;

finalization

FreeAndNil(GlPythonEngine);

end.
